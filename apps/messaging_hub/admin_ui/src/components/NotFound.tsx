import { Link } from "react-router-dom";
import { SearchX } from "lucide-react";

export default function NotFound() {
  return (
    <div className="glass-panel p-8 text-center">
      <SearchX size={40} className="mx-auto text-primary-500" />
      <h2 className="text-2xl font-bold mt-4">ページが見つかりません</h2>
      <p className="text-muted mt-2">
        指定した画面は存在しないか、移動されました。
      </p>
      <Link
        to="/conversations"
        className="inline-block mt-6 neo-button px-5 py-2"
      >
        チャット画面へ戻る
      </Link>
    </div>
  );
}
