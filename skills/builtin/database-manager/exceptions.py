"""Database Manager 异常定义."""


class DatabaseError(Exception):
    """数据库基础异常."""



class ConnectionError(DatabaseError):
    """连接错误."""



class QueryError(DatabaseError):
    """查询错误."""



class RLSError(DatabaseError):
    """RLS 策略错误."""



class MigrationError(DatabaseError):
    """迁移错误."""



class TransactionError(DatabaseError):
    """事务错误."""



class SchemaError(DatabaseError):
    """Schema 错误."""


