/**
 * ファイルアップロードフック.
 *
 * プログレス付きファイルアップロードを管理。
 */
import { useCallback, useState } from 'react';

export interface UploadState {
  uploading: boolean;
  progress: number;
  error: string | null;
  filename: string | null;
}

export interface UseFileUploadReturn {
  state: UploadState;
  upload: (file: File, handler: (file: File) => Promise<void>) => Promise<void>;
  reset: () => void;
}

const INITIAL_STATE: UploadState = {
  uploading: false,
  progress: 0,
  error: null,
  filename: null,
};

/** ファイルアップロードフック */
export function useFileUpload(): UseFileUploadReturn {
  const [state, setState] = useState<UploadState>(INITIAL_STATE);

  const upload = useCallback(
    async (file: File, handler: (file: File) => Promise<void>) => {
      setState({ uploading: true, progress: 0, error: null, filename: file.name });
      try {
        // プログレスシミュレーション（実際の XHR を使用する場合は別途実装）
        setState((s) => ({ ...s, progress: 30 }));
        await handler(file);
        setState((s) => ({ ...s, progress: 100, uploading: false }));
      } catch (err: unknown) {
        const msg = err instanceof Error ? err.message : 'アップロードに失敗しました';
        setState((s) => ({ ...s, error: msg, uploading: false }));
      }
    },
    [],
  );

  const reset = useCallback(() => setState(INITIAL_STATE), []);

  return { state, upload, reset };
}
