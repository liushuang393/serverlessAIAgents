/**
 * RAG ドキュメントアップローダー.
 *
 * ドラッグ&ドロップ対応のファイルアップロード UI。
 */
import React, { useCallback, useRef, useState } from 'react';

export interface RAGDocumentUploaderProps {
  onUpload: (file: File) => void;
  acceptedTypes?: string[];
  maxSizeMB?: number;
  uploading?: boolean;
}

const DEFAULT_TYPES = ['.pdf', '.txt', '.md', '.csv', '.json', '.html', '.docx'];

/** ドラッグ&ドロップアップローダー */
export const RAGDocumentUploader: React.FC<RAGDocumentUploaderProps> = ({
  onUpload,
  acceptedTypes = DEFAULT_TYPES,
  maxSizeMB = 50,
  uploading = false,
}) => {
  const [isDragging, setIsDragging] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  const validate = useCallback(
    (file: File): string | null => {
      const ext = '.' + file.name.split('.').pop()?.toLowerCase();
      if (!acceptedTypes.includes(ext)) {
        return `対応していないファイル形式です: ${ext}`;
      }
      if (file.size > maxSizeMB * 1024 * 1024) {
        return `ファイルサイズが上限 (${maxSizeMB}MB) を超えています`;
      }
      return null;
    },
    [acceptedTypes, maxSizeMB],
  );

  const handleFile = useCallback(
    (file: File) => {
      setError(null);
      const err = validate(file);
      if (err) {
        setError(err);
        return;
      }
      onUpload(file);
    },
    [validate, onUpload],
  );

  const handleDrop = useCallback(
    (e: React.DragEvent) => {
      e.preventDefault();
      setIsDragging(false);
      const file = e.dataTransfer.files[0];
      if (file) handleFile(file);
    },
    [handleFile],
  );

  return (
    <div className="space-y-2">
      <div
        onDragOver={(e) => {
          e.preventDefault();
          setIsDragging(true);
        }}
        onDragLeave={() => setIsDragging(false)}
        onDrop={handleDrop}
        onClick={() => inputRef.current?.click()}
        className={`relative flex flex-col items-center justify-center gap-2 p-8 rounded-xl border-2 border-dashed cursor-pointer transition-all ${
          isDragging
            ? 'border-indigo-400 bg-indigo-500/10'
            : 'border-slate-600 bg-slate-800/30 hover:border-slate-500 hover:bg-slate-800/50'
        } ${uploading ? 'opacity-50 pointer-events-none' : ''}`}
      >
        <span className="text-3xl text-slate-400">
          {uploading ? '\u23F3' : '\uD83D\uDCC4'}
        </span>
        <p className="text-sm text-slate-300">
          {uploading
            ? 'アップロード中...'
            : 'ファイルをドラッグ&ドロップ、またはクリックして選択'}
        </p>
        <p className="text-xs text-slate-500">
          対応形式: {acceptedTypes.join(', ')} / 最大 {maxSizeMB}MB
        </p>
        <input
          ref={inputRef}
          type="file"
          accept={acceptedTypes.join(',')}
          onChange={(e) => {
            const file = e.target.files?.[0];
            if (file) handleFile(file);
            e.target.value = '';
          }}
          className="hidden"
        />
      </div>
      {error && (
        <p className="text-xs text-rose-400 px-1">{error}</p>
      )}
    </div>
  );
};
