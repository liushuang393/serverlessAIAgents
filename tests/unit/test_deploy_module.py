"""Deploy 模块的完整测试.

覆盖 docker_generator.py, serverless_generator.py, ci_cd_generator.py
"""

import json
import tempfile
import unittest


class TestDockerConfig(unittest.TestCase):
    """DockerConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.deploy.docker_generator import DockerConfig

        config = DockerConfig()
        self.assertEqual(config.app_name, "agentflow-app")
        self.assertEqual(config.python_version, "3.13")
        self.assertEqual(config.port, 8000)
        self.assertEqual(config.entry_point, "main:app")
        self.assertEqual(config.health_check_path, "/health")
        self.assertEqual(config.env_vars, {})
        self.assertEqual(config.extra_packages, [])

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.deploy.docker_generator import DockerConfig

        config = DockerConfig(
            app_name="my-app",
            python_version="3.12",
            port=3000,
            entry_point="app:main",
            env_vars={"DEBUG": "true"},
            extra_packages=["curl"],
        )

        self.assertEqual(config.app_name, "my-app")
        self.assertEqual(config.python_version, "3.12")
        self.assertEqual(config.port, 3000)


class TestGenerateDockerfile(unittest.TestCase):
    """generate_dockerfile 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.docker_generator import DockerConfig, generate_dockerfile

        with tempfile.TemporaryDirectory() as tmpdir:
            config = DockerConfig(python_version="3.13", port=8000)
            path = generate_dockerfile(tmpdir, config)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("FROM python:3.13", content)
            self.assertIn("EXPOSE 8000", content)
            self.assertIn("WORKDIR /app", content)

    def test_with_kwargs(self):
        """使用 kwargs 测试."""
        from agentflow.deploy.docker_generator import generate_dockerfile

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_dockerfile(tmpdir, app_name="custom-app")

            content = path.read_text()
            self.assertIn("custom-app", content)


class TestGenerateDockerCompose(unittest.TestCase):
    """generate_docker_compose 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.docker_generator import generate_docker_compose

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_docker_compose(tmpdir, app_name="myapp", port=8000)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("services:", content)
            self.assertIn("myapp", content)
            self.assertIn("8000:8000", content)

    def test_with_env_vars(self):
        """带环境变量测试."""
        from agentflow.deploy.docker_generator import DockerConfig, generate_docker_compose

        with tempfile.TemporaryDirectory() as tmpdir:
            config = DockerConfig(
                app_name="myapp",
                port=8000,
                env_vars={"DEBUG": "true", "API_KEY": "secret"},
            )
            path = generate_docker_compose(tmpdir, config)

            content = path.read_text()
            self.assertIn("DEBUG", content)


class TestGenerateDockerIgnore(unittest.TestCase):
    """generate_dockerignore 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.docker_generator import generate_dockerignore

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_dockerignore(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("__pycache__", content)
            self.assertIn(".git", content)
            self.assertIn("*.py[cod]", content)
            self.assertIn(".env", content)


class TestServerlessConfig(unittest.TestCase):
    """ServerlessConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.deploy.serverless_generator import ServerlessConfig

        config = ServerlessConfig()
        self.assertEqual(config.app_name, "agentflow-app")
        self.assertEqual(config.python_version, "3.13")
        self.assertEqual(config.entry_point, "main.handler")
        self.assertEqual(config.region, "us-east-1")
        self.assertEqual(config.memory_size, 512)
        self.assertEqual(config.timeout, 30)
        self.assertEqual(config.env_vars, {})

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.deploy.serverless_generator import ServerlessConfig

        config = ServerlessConfig(
            app_name="custom-app",
            python_version="3.12",
            region="ap-northeast-1",
            memory_size=1024,
            timeout=60,
            env_vars={"DEBUG": "true"},
        )

        self.assertEqual(config.app_name, "custom-app")
        self.assertEqual(config.timeout, 60)
        self.assertEqual(config.memory_size, 1024)


class TestGenerateVercelConfig(unittest.TestCase):
    """generate_vercel_config 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.serverless_generator import generate_vercel_config

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_vercel_config(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()
            config = json.loads(content)

            self.assertIn("version", config)
            self.assertIn("builds", config)
            self.assertIn("routes", config)

    def test_with_env_vars(self):
        """带环境变量测试."""
        from agentflow.deploy.serverless_generator import ServerlessConfig, generate_vercel_config

        with tempfile.TemporaryDirectory() as tmpdir:
            serverless_config = ServerlessConfig(env_vars={"API_KEY": "secret"})
            path = generate_vercel_config(tmpdir, serverless_config)

            content = json.loads(path.read_text())
            self.assertIn("env", content)
            self.assertEqual(content["env"]["API_KEY"], "secret")


class TestGenerateAWSLambdaConfig(unittest.TestCase):
    """generate_aws_lambda_config 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.serverless_generator import generate_aws_lambda_config

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_aws_lambda_config(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("service:", content)
            self.assertIn("provider:", content)
            self.assertIn("functions:", content)

    def test_with_config(self):
        """带配置测试."""
        from agentflow.deploy.serverless_generator import (
            ServerlessConfig,
            generate_aws_lambda_config,
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            config = ServerlessConfig(
                app_name="my-func",
                memory_size=1024,
                timeout=60,
            )
            path = generate_aws_lambda_config(tmpdir, config)

            content = path.read_text()
            self.assertIn("my-func", content)
            self.assertIn("1024", content)
            self.assertIn("60", content)


class TestGenerateRequirementsTxt(unittest.TestCase):
    """generate_requirements_txt 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.serverless_generator import generate_requirements_txt

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_requirements_txt(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("fastapi", content)
            self.assertIn("uvicorn", content)
            self.assertIn("mangum", content)

    def test_with_extra_packages(self):
        """带额外包测试."""
        from agentflow.deploy.serverless_generator import generate_requirements_txt

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_requirements_txt(tmpdir, extra_packages=["redis", "celery"])

            content = path.read_text()
            self.assertIn("redis", content)
            self.assertIn("celery", content)


class TestCICDConfig(unittest.TestCase):
    """CICDConfig 的测试."""

    def test_defaults(self):
        """默认值测试."""
        from agentflow.deploy.ci_cd_generator import CICDConfig

        config = CICDConfig()
        self.assertEqual(config.app_name, "agentflow-app")
        self.assertEqual(config.python_version, "3.13")
        self.assertEqual(config.node_version, "20")
        self.assertEqual(config.test_command, "pytest")
        self.assertIn("ruff", config.lint_command)
        self.assertEqual(config.deploy_target, "docker")
        self.assertEqual(config.branches, ["main", "develop"])

    def test_custom_values(self):
        """自定义值测试."""
        from agentflow.deploy.ci_cd_generator import CICDConfig

        config = CICDConfig(
            app_name="my-app",
            python_version="3.12",
            test_command="pytest -v",
            branches=["main"],
        )

        self.assertEqual(config.app_name, "my-app")
        self.assertEqual(config.branches, ["main"])


class TestGenerateGitHubActions(unittest.TestCase):
    """generate_github_actions 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.ci_cd_generator import generate_github_actions

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_github_actions(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("name:", content)
            self.assertIn("on:", content)
            self.assertIn("jobs:", content)
            self.assertIn("pytest", content)

    def test_with_config(self):
        """带配置生成测试."""
        from agentflow.deploy.ci_cd_generator import CICDConfig, generate_github_actions

        with tempfile.TemporaryDirectory() as tmpdir:
            config = CICDConfig(
                python_version="3.11",
                test_command="pytest -v --cov",
            )
            path = generate_github_actions(tmpdir, config)

            content = path.read_text()
            self.assertIn("3.11", content)
            self.assertIn("pytest -v --cov", content)


class TestGenerateGitLabCI(unittest.TestCase):
    """generate_gitlab_ci 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.ci_cd_generator import generate_gitlab_ci

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_gitlab_ci(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("stages:", content)
            self.assertIn("test", content)
            self.assertIn("pytest", content)

    def test_with_config(self):
        """带配置生成测试."""
        from agentflow.deploy.ci_cd_generator import CICDConfig, generate_gitlab_ci

        with tempfile.TemporaryDirectory() as tmpdir:
            config = CICDConfig(python_version="3.12")
            path = generate_gitlab_ci(tmpdir, config)

            content = path.read_text()
            self.assertIn("3.12", content)


class TestGeneratePreCommitConfig(unittest.TestCase):
    """generate_pre_commit_config 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy.ci_cd_generator import generate_pre_commit_config

        with tempfile.TemporaryDirectory() as tmpdir:
            path = generate_pre_commit_config(tmpdir)

            self.assertTrue(path.exists())
            content = path.read_text()

            self.assertIn("repos:", content)
            self.assertIn("ruff", content)
            self.assertIn("mypy", content)


class TestDeployModuleExports(unittest.TestCase):
    """deploy 模块导出的测试."""

    def test_imports(self):
        """导入测试."""
        from agentflow.deploy import (
            generate_all,
            generate_docker_compose,
            generate_dockerfile,
        )

        # All should be callable
        self.assertTrue(callable(generate_dockerfile))
        self.assertTrue(callable(generate_docker_compose))
        self.assertTrue(callable(generate_all))


class TestGenerateAll(unittest.TestCase):
    """generate_all 函数的测试."""

    def test_basic_generation(self):
        """基本生成测试."""
        from agentflow.deploy import generate_all

        with tempfile.TemporaryDirectory() as tmpdir:
            result = generate_all(tmpdir, app_name="myapp")

            self.assertIsInstance(result, dict)
            self.assertIn("dockerfile", result)
            self.assertIn("docker_compose", result)
            self.assertIn("github_actions", result)


if __name__ == "__main__":
    unittest.main()
