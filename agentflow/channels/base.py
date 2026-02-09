"""Message Channel Adapter Base Classes.

定义消息平台适配器的统一接口，支持多种消息平台集成。

设计原则：
- 统一抽象：所有平台遵循相同接口
- 松耦合：平台细节封装在适配器内
- 异步优先：所有 I/O 操作必须异步
- 类型安全：100% 类型注解
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class MessageType(str, Enum):
    """消息类型."""

    TEXT = "text"
    IMAGE = "image"
    AUDIO = "audio"
    VIDEO = "video"
    FILE = "file"
    LOCATION = "location"
    STICKER = "sticker"
    SYSTEM = "system"


@dataclass
class MessageMetadata:
    """消息元数据.

    Attributes:
        platform: 平台名称（telegram, slack, discord 等）
        platform_message_id: 平台原始消息 ID
        thread_id: 会话线程 ID（可选）
        reply_to_message_id: 回复的消息 ID（可选）
        attachments: 附件列表
        raw_data: 原始平台数据
    """

    platform: str
    platform_message_id: str
    thread_id: str | None = None
    reply_to_message_id: str | None = None
    attachments: list[dict[str, Any]] = field(default_factory=list)
    raw_data: dict[str, Any] = field(default_factory=dict)


@dataclass
class UserInfo:
    """用户信息.

    Attributes:
        user_id: 用户 ID
        username: 用户名（可选）
        display_name: 显示名称
        avatar_url: 头像 URL（可选）
        is_bot: 是否为机器人
        metadata: 平台特定元数据
    """

    user_id: str
    username: str | None = None
    display_name: str = ""
    avatar_url: str | None = None
    is_bot: bool = False
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class ChannelMessage:
    """统一消息格式.

    Attributes:
        message_id: 消息 ID（框架生成）
        user_id: 发送用户 ID
        channel_id: 频道/群组 ID
        text: 消息文本
        message_type: 消息类型
        timestamp: 时间戳
        metadata: 消息元数据
        user_info: 用户信息（可选）
    """

    message_id: str
    user_id: str
    channel_id: str
    text: str
    message_type: MessageType = MessageType.TEXT
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: MessageMetadata | None = None
    user_info: UserInfo | None = None


class MessageChannelAdapter(ABC):
    """消息平台适配器基类.

    所有消息平台适配器必须继承此类并实现抽象方法。

    Example:
        >>> class MyPlatformAdapter(MessageChannelAdapter):
        ...     @property
        ...     def platform_name(self) -> str:
        ...         return "myplatform"
        ...
        ...     async def send_message(self, channel_id: str, text: str, **kwargs) -> str:
        ...         # 实现发送逻辑
        ...         return "message_id"
        ...
        ...     async def send_typing_indicator(self, channel_id: str) -> None:
        ...         # 实现输入指示器
        ...         pass
        ...
        ...     async def get_user_info(self, user_id: str) -> UserInfo:
        ...         # 实现用户信息获取
        ...         return UserInfo(user_id=user_id)
    """

    @property
    @abstractmethod
    def platform_name(self) -> str:
        """平台名称（telegram, slack, discord 等）.

        Returns:
            平台名称
        """

    @abstractmethod
    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """发送消息到平台.

        Args:
            channel_id: 频道/用户 ID
            text: 消息文本
            **kwargs: 平台特定参数（reply_to, attachments 等）

        Returns:
            消息 ID

        Raises:
            Exception: 发送失败时抛出异常
        """

    @abstractmethod
    async def send_typing_indicator(self, channel_id: str) -> None:
        """发送"正在输入"指示器.

        Args:
            channel_id: 频道/用户 ID
        """

    @abstractmethod
    async def get_user_info(self, user_id: str) -> UserInfo:
        """获取用户信息.

        Args:
            user_id: 用户 ID

        Returns:
            用户信息

        Raises:
            Exception: 获取失败时抛出异常
        """

    async def send_image(
        self,
        channel_id: str,
        image_url: str,
        caption: str | None = None,
        **kwargs: Any,
    ) -> str:
        """发送图片（可选实现）.

        Args:
            channel_id: 频道/用户 ID
            image_url: 图片 URL
            caption: 图片说明
            **kwargs: 平台特定参数

        Returns:
            消息 ID
        """
        msg = f"Image sending not implemented for {self.platform_name}"
        raise NotImplementedError(msg)

    async def send_file(
        self,
        channel_id: str,
        file_url: str,
        filename: str | None = None,
        **kwargs: Any,
    ) -> str:
        """发送文件（可选实现）.

        Args:
            channel_id: 频道/用户 ID
            file_url: 文件 URL
            filename: 文件名
            **kwargs: 平台特定参数

        Returns:
            消息 ID
        """
        msg = f"File sending not implemented for {self.platform_name}"
        raise NotImplementedError(msg)

    async def delete_message(
        self,
        channel_id: str,
        message_id: str,
    ) -> bool:
        """删除消息（可选实现）.

        Args:
            channel_id: 频道/用户 ID
            message_id: 消息 ID

        Returns:
            是否成功删除
        """
        return False

    async def edit_message(
        self,
        channel_id: str,
        message_id: str,
        new_text: str,
        **kwargs: Any,
    ) -> bool:
        """编辑消息（可选实现）.

        Args:
            channel_id: 频道/用户 ID
            message_id: 消息 ID
            new_text: 新文本
            **kwargs: 平台特定参数

        Returns:
            是否成功编辑
        """
        return False
