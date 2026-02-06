import pytest
from agentflow.security.evidence_collector import (
    SystemEvidence,
    FileEvidence,
    NetworkCallEvidence,
)


def test_system_evidence_creation():
    evidence = SystemEvidence(
        exit_code=0,
        stdout="Success",
        stderr="",
        execution_time_ms=150.0,
    )
    assert evidence.exit_code == 0
    assert evidence.evidence_id.startswith("ev-")


def test_system_evidence_default_values():
    evidence = SystemEvidence()
    assert evidence.evidence_id.startswith("ev-")
    assert evidence.exit_code is None
    assert evidence.stdout is None
    assert evidence.stderr is None
    assert evidence.execution_time_ms == 0.0
    assert evidence.produced_files == []
    assert evidence.network_calls == []


def test_file_evidence():
    file_ev = FileEvidence(
        path="/tmp/output.txt",
        operation="write",
        size_bytes=1024,
        checksum="abc123",
    )
    assert file_ev.path == "/tmp/output.txt"
    assert file_ev.operation == "write"
    assert file_ev.exists_after is True


def test_file_evidence_delete():
    file_ev = FileEvidence(
        path="/tmp/deleted.txt",
        operation="delete",
        exists_after=False,
    )
    assert file_ev.exists_after is False


def test_network_call_evidence():
    net_ev = NetworkCallEvidence(
        host="api.example.com",
        port=443,
        protocol="https",
        direction="outbound",
        bytes_sent=100,
        bytes_received=500,
    )
    assert net_ev.host == "api.example.com"
    assert net_ev.direction == "outbound"
    assert net_ev.allowed is True


def test_network_call_evidence_blocked():
    net_ev = NetworkCallEvidence(
        host="malicious.com",
        port=80,
        protocol="http",
        direction="outbound",
        allowed=False,
    )
    assert net_ev.allowed is False


def test_system_evidence_with_files():
    file1 = FileEvidence(
        path="/tmp/file1.txt",
        operation="create",
        size_bytes=100,
    )
    file2 = FileEvidence(
        path="/tmp/file2.txt",
        operation="write",
        size_bytes=200,
    )
    evidence = SystemEvidence(
        exit_code=0,
        stdout="Created files",
        produced_files=[file1, file2],
    )
    assert len(evidence.produced_files) == 2


def test_system_evidence_with_network():
    net_call = NetworkCallEvidence(
        host="localhost",
        port=8080,
        protocol="http",
        direction="outbound",
    )
    evidence = SystemEvidence(
        exit_code=0,
        network_calls=[net_call],
    )
    assert len(evidence.network_calls) == 1
