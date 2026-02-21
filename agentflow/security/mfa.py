import pyotp


class TimeBasedMFA:
    """Time-based One-Time Password (TOTP) MFA implementation using pyotp."""

    @staticmethod
    def generate_secret() -> str:
        """Generate a random base32 MFA secret."""
        return pyotp.random_base32()

    @staticmethod
    def get_provisioning_uri(secret: str, username: str, issuer_name: str) -> str:
        """Generate the OTP provisioning URI for QR codes."""
        return pyotp.TOTP(secret).provisioning_uri(name=username, issuer_name=issuer_name)

    @staticmethod
    def verify_totp(secret: str, code: str) -> bool:
        """Verify the TOTP code against the secret."""
        if not secret or not code:
            return False
        totp = pyotp.TOTP(secret)
        return totp.verify(code)
