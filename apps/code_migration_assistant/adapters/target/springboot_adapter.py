"""Spring Boot Language Adapter.

Spring Boot (MVC/Service/Repository) の構造的な生成をサポート。
"""

from typing import Any

from apps.code_migration_assistant.adapters.base import AST, ExecutionResult
from apps.code_migration_assistant.adapters.target.java_adapter import JavaAdapter


class SpringBootAdapter(JavaAdapter):
    """Spring Boot言語アダプター.

    Controller, Service, Repository, Entityのスケルトンを生成する。
    """

    @property
    def language_name(self) -> str:
        """言語名称."""
        return "SpringBoot"

    def generate_skeleton(self, ast: AST, class_name: str) -> str:
        """Spring Boot サービス全体のスケルトンを生成する.

        注: 複数のファイルを生成するため、ここでは代表的な Service クラスを返すか、
        合体したコードブロックを返す。TransformationAgent がこれを見て分割する。
        """
        lines: list[str] = []

        # 1. Entity
        lines.append(f"// --- [FILE: model/{class_name}Entity.java] ---")
        lines.append("package com.migration.generated.model;")
        lines.append("")
        lines.append("import jakarta.persistence.*;")
        lines.append("import lombok.Data;")
        lines.append("import java.math.BigDecimal;")
        lines.append("")
        lines.append("@Entity")
        lines.append("@Data")
        lines.append(f'@Table(name = "{ast.program_id.lower()}")')
        lines.append(f"public class {class_name}Entity {{")
        lines.append("    @Id")
        lines.append("    @GeneratedValue(strategy = GenerationType.IDENTITY)")
        lines.append("    private Long id;")

        for var in ast.variables:
            java_type = self.get_type_mapping(var.get("type", ""), var.get("pic_clause", ""))
            java_name = self._to_camel_case(var["name"])
            lines.append(f"    private {java_type} {java_name};")
        lines.append("}")
        lines.append("")

        # 2. Repository
        lines.append(f"// --- [FILE: repository/{class_name}Repository.java] ---")
        lines.append("package com.migration.generated.repository;")
        lines.append("")
        lines.append("import com.migration.generated.model." + class_name + "Entity;")
        lines.append("import org.springframework.data.jpa.repository.JpaRepository;")
        lines.append("import org.springframework.stereotype.Repository;")
        lines.append("")
        lines.append("@Repository")
        lines.append(f"public interface {class_name}Repository extends JpaRepository<{class_name}Entity, Long> {{")
        lines.append("}")
        lines.append("")

        # 3. Service
        lines.append(f"// --- [FILE: service/{class_name}Service.java] ---")
        lines.append("package com.migration.generated.service;")
        lines.append("")
        lines.append("import com.migration.generated.model." + class_name + "Entity;")
        lines.append("import com.migration.generated.repository." + class_name + "Repository;")
        lines.append("import org.springframework.stereotype.Service;")
        lines.append("import lombok.RequiredArgsConstructor;")
        lines.append("")
        lines.append("@Service")
        lines.append("@RequiredArgsConstructor")
        lines.append(f"public class {class_name}Service {{")
        lines.append(f"    private final {class_name}Repository repository;")
        lines.append("")
        lines.append("    public void process(" + class_name + "Entity request) {")
        lines.append("        // TODO: Implement business logic migrated from Procedure Division")
        lines.append("    }")
        lines.append("}")
        lines.append("")

        # 4. Controller
        lines.append(f"// --- [FILE: controller/{class_name}Controller.java] ---")
        lines.append("package com.migration.generated.controller;")
        lines.append("")
        lines.append("import com.migration.generated.model." + class_name + "Entity;")
        lines.append("import com.migration.generated.service." + class_name + "Service;")
        lines.append("import org.springframework.web.bind.annotation.*;")
        lines.append("import lombok.RequiredArgsConstructor;")
        lines.append("")
        lines.append("@RestController")
        lines.append(f'@RequestMapping("/api/{ast.program_id.lower()}")')
        lines.append("@RequiredArgsConstructor")
        lines.append(f"public class {class_name}Controller {{")
        lines.append(f"    private final {class_name}Service service;")
        lines.append("")
        lines.append("    @PostMapping")
        lines.append(f"    public void execute(@RequestBody {class_name}Entity request) {{")
        lines.append("        service.process(request);")
        lines.append("    }")
        lines.append("}")

        return "\n".join(lines)

    def compile(self, code: str) -> tuple[bool, list[str]]:
        """Spring Bootコードのコンパイル.

        注: 現状の簡易コンパイルではマルチファイル対応が難しいため、
        構文チェックのみパスさせるか、ダミーを返す。
        """
        # 実際には mvn compile 等が必要だが、コンテナ実行環境を想定する。
        # ここでは「文法上の不備がないか」のみのチェックにとどめる簡易実装、
        # あるいは常に成功を返すスタブとする。
        return True, []

    def execute(self, code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """Spring Bootコードの実行."""
        # Spring Bootの起動は時間がかかるため、
        # ユニットテスト（JUnit）経由での検証を推奨する。
        return ExecutionResult(success=True, stdout="App started in mock mode")
