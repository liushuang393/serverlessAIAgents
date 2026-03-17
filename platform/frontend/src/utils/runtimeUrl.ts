const LOOPBACK_HOSTS = new Set(["localhost", "127.0.0.1", "0.0.0.0"]);

function getBrowserAccessHost(): string | null {
  if (typeof window === "undefined" || !window.location.hostname) {
    return null;
  }
  return window.location.hostname;
}

function formatHost(host: string): string {
  return host.includes(":") && !host.startsWith("[") ? `[${host}]` : host;
}

export function rewriteLoopbackUrl(
  rawUrl: string | null | undefined,
): string | null {
  if (!rawUrl) {
    return null;
  }

  const accessHost = getBrowserAccessHost();
  if (!accessHost) {
    return rawUrl;
  }

  try {
    const url = new URL(rawUrl);
    if (!LOOPBACK_HOSTS.has(url.hostname)) {
      return rawUrl;
    }

    url.hostname = formatHost(accessHost);
    return url.toString();
  } catch {
    return rawUrl;
  }
}

export function resolveRuntimeUrl(
  preferredUrl: string | null | undefined,
  fallbackPort?: number | null,
  defaultPath = "",
): string | null {
  const resolved = rewriteLoopbackUrl(preferredUrl);
  if (resolved) {
    return resolved;
  }
  if (fallbackPort == null) {
    return null;
  }
  return rewriteLoopbackUrl(`http://localhost:${fallbackPort}${defaultPath}`);
}
