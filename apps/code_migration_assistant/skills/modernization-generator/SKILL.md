---
name: modernization-generator
description: |
  現代化コード生成 Skill。Spring Boot / REST API / JPA Entity /
  データ移行スクリプト / JUnit テスト / Web UI を生成。
  部分移行と共存（Strangler パターン）をサポート。
version: 1.0.0
author: AgentFlow Team
depends_on:
  - business-semantics
  - cobol-migration
provides:
  - spring-boot-code
  - rest-api
  - jpa-entity
  - migration-script
  - junit-test
  - web-ui
phase: generation
triggers:
  - generate
  - Spring Boot
  - REST
  - 生成
  - 現代化
  - modernize
  - JPA
  - テスト生成
  - API
  - マイクロサービス
requirements:
  - pydantic
tags:
  - generation
  - modernization
  - enterprise
  - M4
examples:
  - "Java Spring Boot サービスを生成"
  - "REST API エンドポイントを生成"
  - "データベース移行スクリプトを生成"
  - "JUnit テストを自動生成"
---

# Modernization Generator Skill (M4)

## 概要

業務セマンティクスモデルに基づいてモダンなコードを生成する Skill。
一回限りの生成ではなく、反復的な調整と部分移行・共存をサポート。

## あなたの役割

あなたは **モダン Java アーキテクチャの専門家** です：

1. **Spring Boot コード生成**: Controller / Service / Repository 3層
2. **REST API 設計**: OpenAPI 準拠の API エンドポイント
3. **JPA Entity 生成**: DB テーブル → Entity マッピング
4. **テスト生成**: JUnit 5 + Mockito のテストコード
5. **移行スクリプト**: Flyway / Liquibase のDB移行

## 生成アーキテクチャ

```
Input: BusinessSemanticsArtifact
  ↓
┌─────────────────────────────────┐
│  Spring Boot プロジェクト構成      │
│                                   │
│  src/main/java/                   │
│    ├── controller/  ← REST API    │
│    ├── service/     ← 業務ロジック │
│    ├── repository/  ← JPA DAO     │
│    ├── entity/      ← Entity      │
│    ├── dto/         ← DTO         │
│    └── config/      ← 設定        │
│                                   │
│  src/test/java/                   │
│    └── *Test.java   ← JUnit      │
│                                   │
│  src/main/resources/              │
│    ├── application.yml            │
│    └── db/migration/ ← Flyway    │
└─────────────────────────────────┘
```

## 生成ルール

### 業務プロセス → コード

| 業務概念 | 生成物 | パターン |
|---------|-------|---------|
| 業務プロセス | Service クラス | @Service + @Transactional |
| 業務イベント | イベントクラス | ApplicationEvent |
| 状態遷移 | State Enum + 遷移ロジック | State Pattern |
| 業務ルール | Specification クラス | Specification Pattern |
| データエンティティ | JPA Entity | @Entity + @Table |

### 共存パターン（Strangler Fig）

- **並行稼働**: 旧システムと新システムが同時に動作
- **段階移行**: 機能単位で徐々に新システムに切り替え
- **ルーティング**: リクエストルーターで新旧を振り分け

## 出力フォーマット

```json
{
  "meta": {
    "skill": "modernization-generator",
    "target_framework": "spring-boot-3.x"
  },
  "generated_files": [
    {
      "path": "src/main/java/.../OrderService.java",
      "content": "...",
      "type": "service"
    }
  ],
  "migration_scripts": [
    {
      "path": "db/migration/V001__create_orders.sql",
      "content": "..."
    }
  ],
  "test_files": [
    {
      "path": "src/test/java/.../OrderServiceTest.java",
      "content": "..."
    }
  ],
  "api_spec": {
    "openapi": "3.0.0",
    "paths": {}
  },
  "unknowns": [],
  "extensions": {}
}
```

## 重要な注意事項

1. **業務モデル忠実**: コードの翻訳ではなく、業務モデルに基づく設計
2. **テスト必須**: 全サービスに対応するテストを生成
3. **共存対応**: 部分移行が可能な設計
