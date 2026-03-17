param(
    [string]$HostIP = "",
    [string]$PortsCsv = "",
    [ValidateSet("auto", "firewall-only", "auto-portproxy")]
    [string]$Mode = "firewall-only",
    [switch]$DryRun
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"
Set-Location $env:SystemRoot

$WindowsNetsh = Join-Path $env:SystemRoot "System32\netsh.exe"
$WindowsWsl = Join-Path $env:SystemRoot "System32\wsl.exe"

function Test-IsAdmin {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = [Security.Principal.WindowsPrincipal]::new($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Get-ListeningPorts {
    $ports = @{}
    $lines = cmd.exe /c "cd /d %SystemRoot% && netstat -ano -p tcp"
    foreach ($line in $lines) {
        if ($line -notmatch "LISTENING") {
            continue
        }
        $trimmed = ($line -replace "\s+", " ").Trim()
        $parts = $trimmed.Split(" ")
        if ($parts.Count -lt 2) {
            continue
        }
        $localAddress = $parts[1]
        if ($localAddress -match ":(\d+)$") {
            $ports[[int]$matches[1]] = $true
        }
    }
    return $ports
}

function Ensure-FirewallRule {
    param(
        [int]$Port,
        [bool]$WhatIfMode
    )

    $ruleName = "AgentFlow LAN Demo $Port"
    $existing = & $WindowsNetsh advfirewall firewall show rule name="$ruleName"
    if (($existing | Out-String) -notmatch "No rules match") {
        return "exists"
    }

    if ($WhatIfMode) {
        return "would-create"
    }

    & $WindowsNetsh advfirewall firewall add rule `
        name="$ruleName" `
        dir=in `
        action=allow `
        profile=private `
        protocol=TCP `
        localport=$Port `
        remoteip=LocalSubnet | Out-Null
    return "created"
}

function Get-WslIPv4 {
    try {
        $ip = & $WindowsWsl -e sh -lc "hostname -I" 2>$null
        $trimmed = ($ip | Out-String).Trim()
        if ([string]::IsNullOrWhiteSpace($trimmed)) {
            return $null
        }
        return $trimmed.Split(" ", [System.StringSplitOptions]::RemoveEmptyEntries)[0]
    } catch {
        return $null
    }
}

function Ensure-PortProxy {
    param(
        [string]$ListenAddress,
        [int]$Port,
        [string]$ConnectAddress,
        [bool]$WhatIfMode
    )

    $current = & $WindowsNetsh interface portproxy show v4tov4
    $needle = "$ListenAddress`:$Port"
    if ($current -match [regex]::Escape($needle)) {
        if ($WhatIfMode) {
            return "exists"
        }
        & $WindowsNetsh interface portproxy delete v4tov4 listenaddress=$ListenAddress listenport=$Port | Out-Null
    }

    if ($WhatIfMode) {
        return "would-create"
    }

    & $WindowsNetsh interface portproxy add v4tov4 `
        listenaddress=$ListenAddress `
        listenport=$Port `
        connectaddress=$ConnectAddress `
        connectport=$Port | Out-Null
    return "created"
}

if ((-not $DryRun) -and (-not (Test-IsAdmin))) {
    throw "Run this PowerShell script as Administrator."
}

$Ports = @()
foreach ($item in $PortsCsv.Split(",")) {
    if ([string]::IsNullOrWhiteSpace($item)) {
        continue
    }
    $Ports += [int]$item.Trim()
}

if ($Ports.Count -eq 0) {
    throw "Specify at least one TCP port via -PortsCsv."
}

$listenMap = Get-ListeningPorts
$wslIp = Get-WslIPv4
$results = @()

foreach ($port in ($Ports | Sort-Object -Unique)) {
    $firewallState = Ensure-FirewallRule -Port $port -WhatIfMode:$DryRun
    $listening = $listenMap.ContainsKey($port)
    $proxyState = "skipped"

    $shouldProxy = $false
    if ($Mode -eq "auto-portproxy") {
        $shouldProxy = $true
    } elseif ($Mode -eq "auto" -and -not $listening -and $null -ne $wslIp) {
        $shouldProxy = $true
    }

    if ($shouldProxy) {
        if ([string]::IsNullOrWhiteSpace($HostIP)) {
            throw "Specify -HostIP when portproxy creation is required."
        }
        if ($null -eq $wslIp) {
            throw "Failed to detect the current WSL IPv4 address."
        }
        $proxyState = Ensure-PortProxy -ListenAddress $HostIP -Port $port -ConnectAddress $wslIp -WhatIfMode:$DryRun
    }

    $results += [pscustomobject]@{
        port = $port
        listening = $listening
        firewall = $firewallState
        portproxy = $proxyState
        host_ip = $HostIP
        wsl_ip = $wslIp
    }
}

$results | Format-Table -AutoSize | Out-String | Write-Host

$summary = [pscustomobject]@{
    mode = $Mode
    host_ip = $HostIP
    wsl_ip = $wslIp
    total_ports = $results.Count
    dry_run = [bool]$DryRun
}

$summary | ConvertTo-Json -Depth 3 | Write-Host
