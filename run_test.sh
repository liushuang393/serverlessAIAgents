#!/bin/bash
# テスト実行スクリプト
trap '' INT
cd /home/liush/projects/serverlessAIAgents
export PYTHONPATH=/home/liush/projects/serverlessAIAgents
PY=/home/liush/miniconda3/envs/agentflow/bin/python

# 残存pytestプロセスをkill
ps aux | grep "pytest tests" | grep -v grep | awk '{print $2}' | xargs kill 2>/dev/null
sleep 2

# テスト実行
$PY -m pytest tests/ --tb=line -q --no-header > /home/liush/projects/serverlessAIAgents/test_out.txt 2>&1
echo "EXIT=$?" >> /home/liush/projects/serverlessAIAgents/test_out.txt
echo "DONE" >> /home/liush/projects/serverlessAIAgents/test_out.txt

