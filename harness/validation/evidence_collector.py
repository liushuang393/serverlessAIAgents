"""System evidence collection for Agent OS.

Provides verifiable system-level execution evidence.
The key principle: Don't trust model claims - only trust system evidence.
"""

import uuid
from datetime import UTC, datetime
from typing import Literal

from pydantic import BaseModel, Field


class FileEvidence(BaseModel):
    """Evidence of file system operations.

    Records what happened to files during execution,
    providing verifiable proof of file operations.

    Attributes:
        path: Path to the file
        operation: Type of operation performed
        size_bytes: File size after operation (if applicable)
        checksum: File checksum for verification
        timestamp: When the operation occurred
        exists_after: Whether file exists after operation
    """

    path: str
    operation: Literal["read", "write", "delete", "create"]
    size_bytes: int | None = None
    checksum: str | None = None
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    exists_after: bool = True

    model_config = {"frozen": False}


class NetworkCallEvidence(BaseModel):
    """Evidence of network operations.

    Records network calls made during execution,
    providing verifiable proof of network activity.

    Attributes:
        host: Target host
        port: Target port
        protocol: Protocol used
        direction: Direction of the call
        bytes_sent: Bytes sent
        bytes_received: Bytes received
        allowed: Whether the call was allowed by policy
    """

    host: str
    port: int
    protocol: str
    direction: Literal["inbound", "outbound"]
    bytes_sent: int = 0
    bytes_received: int = 0
    allowed: bool = True

    model_config = {"frozen": False}


class SystemEvidence(BaseModel):
    """Verifiable system-level execution evidence.

    Collects evidence from actual system execution, not model claims.
    This is the foundation of the "don't trust model claims" principle.

    Attributes:
        evidence_id: Unique identifier for this evidence
        timestamp: When the evidence was collected
        exit_code: Process exit code (if applicable)
        stdout: Standard output
        stderr: Standard error
        execution_time_ms: Execution time in milliseconds
        produced_files: Files created/modified during execution
        network_calls: Network calls made during execution
    """

    evidence_id: str = Field(default_factory=lambda: f"ev-{uuid.uuid4().hex[:12]}")
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    exit_code: int | None = None
    stdout: str | None = None
    stderr: str | None = None
    execution_time_ms: float = 0.0
    produced_files: list[FileEvidence] = Field(default_factory=list)
    network_calls: list[NetworkCallEvidence] = Field(default_factory=list)

    model_config = {"frozen": False}

    def was_successful(self) -> bool:
        """Check if the execution was successful.

        Returns:
            True if exit_code is 0 or None (no process)
        """
        return self.exit_code is None or self.exit_code == 0

    def had_network_activity(self) -> bool:
        """Check if there was any network activity.

        Returns:
            True if any network calls were made
        """
        return len(self.network_calls) > 0

    def had_blocked_network(self) -> bool:
        """Check if any network calls were blocked.

        Returns:
            True if any network calls were blocked by policy
        """
        return any(not nc.allowed for nc in self.network_calls)

    def get_file_operations(self, operation: str | None = None) -> list[FileEvidence]:
        """Get file operations, optionally filtered by type.

        Args:
            operation: Filter by operation type (read, write, delete, create)

        Returns:
            List of file evidence matching the filter
        """
        if operation is None:
            return self.produced_files
        return [f for f in self.produced_files if f.operation == operation]

    def add_file_evidence(
        self,
        path: str,
        operation: Literal["read", "write", "delete", "create"],
        size_bytes: int | None = None,
        checksum: str | None = None,
        exists_after: bool = True,
    ) -> None:
        """Add file evidence to this collection.

        Args:
            path: Path to the file
            operation: Type of operation
            size_bytes: File size after operation
            checksum: File checksum
            exists_after: Whether file exists after operation
        """
        self.produced_files.append(
            FileEvidence(
                path=path,
                operation=operation,
                size_bytes=size_bytes,
                checksum=checksum,
                exists_after=exists_after,
            )
        )

    def add_network_evidence(
        self,
        host: str,
        port: int,
        protocol: str,
        direction: Literal["inbound", "outbound"],
        bytes_sent: int = 0,
        bytes_received: int = 0,
        allowed: bool = True,
    ) -> None:
        """Add network call evidence to this collection.

        Args:
            host: Target host
            port: Target port
            protocol: Protocol used
            direction: Direction of the call
            bytes_sent: Bytes sent
            bytes_received: Bytes received
            allowed: Whether the call was allowed
        """
        self.network_calls.append(
            NetworkCallEvidence(
                host=host,
                port=port,
                protocol=protocol,
                direction=direction,
                bytes_sent=bytes_sent,
                bytes_received=bytes_received,
                allowed=allowed,
            )
        )
