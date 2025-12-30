"""Weather Agent - 天気情報を取得するエージェント."""

from __future__ import annotations

import random
from typing import Any, ClassVar

from agentflow.core.agent_block import AgentBlock


class WeatherAgent(AgentBlock):
    """天気情報を取得するエージェント.

    このエージェントは指定された場所の天気情報を取得します。
    実際の API 呼び出しの代わりに、デモ用のダミーデータを返します。
    """

    # 天気の状態リスト
    CONDITIONS: ClassVar[list[str]] = [
        "晴れ",
        "曇り",
        "雨",
        "雪",
        "霧",
        "雷雨",
    ]

    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        print("🌤️ Weather Agent を初期化しました")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """天気情報を取得.

        Args:
            input_data: 入力データ
                - location (str): 場所
                - units (str, optional): 温度単位 (celsius または fahrenheit)

        Returns:
            天気情報
                - temperature (float): 気温
                - condition (str): 天気の状態
                - humidity (int): 湿度 (%)
                - wind_speed (float): 風速 (m/s)
                - location (str): 場所

        Raises:
            ValueError: location が指定されていない場合
        """
        location = input_data.get("location")
        if not location:
            msg = "location は必須です"
            raise ValueError(msg)

        units = input_data.get("units", "celsius")

        # ダミーデータを生成
        # 実際の実装では、OpenWeatherMap などの API を呼び出します
        temperature = self._generate_temperature(units)
        condition = random.choice(self.CONDITIONS)
        humidity = random.randint(30, 90)
        wind_speed = round(random.uniform(0, 20), 1)

        return {
            "temperature": temperature,
            "condition": condition,
            "humidity": humidity,
            "wind_speed": wind_speed,
            "location": location,
            "units": units,
        }

    def _generate_temperature(self, units: str) -> float:
        """ダミーの気温を生成.

        Args:
            units: 温度単位

        Returns:
            気温
        """
        if units == "fahrenheit":
            # 華氏: 32°F ~ 95°F
            return round(random.uniform(32, 95), 1)
        # 摂氏: 0°C ~ 35°C
        return round(random.uniform(0, 35), 1)

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        print("🧹 Weather Agent をクリーンアップしました")
        await super().cleanup()


# エージェントのエントリーポイント
if __name__ == "__main__":
    import asyncio

    async def main() -> None:
        """メイン関数."""
        async with WeatherAgent(metadata_path="agent.yaml") as agent:
            # 東京の天気を取得
            result = await agent.run(
                {
                    "location": "東京",
                    "units": "celsius",
                }
            )

            print(f"\n📍 場所: {result['location']}")
            print(f"🌡️ 気温: {result['temperature']}°C")
            print(f"☁️ 天気: {result['condition']}")
            print(f"💧 湿度: {result['humidity']}%")
            print(f"💨 風速: {result['wind_speed']} m/s")

    asyncio.run(main())
