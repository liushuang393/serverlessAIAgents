# agentflow/（Kernel フレームワーク）

`agentflow/` は AgentFlow の Kernel 実装です。Orchestration / Agent / Tool / Protocol を安定境界として提供し、`apps/`（製品アプリ）と `plugins/`（拡張）から利用されます。

## 開発環境（インストール）

リポジトリルートで統一スクリプトを実行します。

```bash
cd <repo-root>
bash setup_dev.sh
```

手動で行う場合:

```bash
conda activate agentflow
pip install -e ".[dev,apps]"
```

## 開発起動

Platform（推奨: Control Plane）:

```bash
conda activate agentflow
python -m apps.platform.main serve
```

Studio（Kernel 側の開発サーバー）:

```bash
cd <repo-root>
make dev-backend
make dev-frontend
```

## テスト/静的チェック（統一スクリプト）

```bash
cd <repo-root>
./check.sh lint
./check.sh type-check
./check.sh test
```

または:

```bash
cd <repo-root>
make check-all
```

## 本番ビルド

Python パッケージ:

```bash
cd <repo-root>
make build
```

フロントエンド（studio/）:

```bash
cd <repo-root>
make build-frontend
```

