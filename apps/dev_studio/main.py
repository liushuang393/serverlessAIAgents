"""Developer Studio Main Server.
"""
import uvicorn
from fastapi import FastAPI
from apps.dev_studio import code_intelligence, codegen, wizard

app = FastAPI(title="Developer Studio API", version="1.0.0")

@app.get("/health")
async def health_check():
    """ヘルスチェックエンドポイント."""
    return {
        "status": "healthy",
        "service": "dev_studio",
        "version": "1.0.0",
        "capabilities": ["code_intelligence", "codegen", "wizard"]
    }

@app.get("/")
async def root():
    """ルートエンドポイント."""
    return {"message": "Developer Studio API is running."}

# 将来的にルーターを追加するためのプレースホルダー
# app.include_router(code_intelligence.router, prefix="/code")
# app.include_router(codegen.router, prefix="/codegen")
# app.include_router(wizard.router, prefix="/wizard")

if __name__ == "__main__":
    uvicorn.run("apps.dev_studio.main:app", host="0.0.0.0", port=8011, reload=True)
