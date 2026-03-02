      *----------------------------------------------------------------
      * SAMPLE: 顧客情報処理サンプルプログラム
      * COBOL→Java Spring Boot 変換テスト用
      *
      * 処理概要:
      *   1. 入力パラメータから顧客IDを受け取る
      *   2. 顧客マスタ（DB）から情報を取得
      *   3. 金額計算（消費税加算）
      *   4. 処理結果を返す
      *
      * AI変換テストポイント:
      *   - BigDecimal使用（PIC S9(9)V9(2)）
      *   - EXEC SQL (SELECT, UPDATE)
      *   - 条件分岐 (IF/EVALUATE)
      *   - エラーハンドリング (SQLCODE)
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROC.
       AUTHOR.     MIGRATION-TEAM.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * 顧客入力情報
       01  WS-INPUT.
           05  WS-CUSTOMER-ID    PIC X(10).
           05  WS-AMOUNT         PIC S9(9)V9(2) COMP-3.
           05  WS-PROCESS-TYPE   PIC X(01).
               88  WS-TYPE-QUERY  VALUE 'Q'.
               88  WS-TYPE-UPDATE VALUE 'U'.
      *
      * 顧客マスタ情報
       01  WS-CUSTOMER-MASTER.
           05  WS-CUST-NAME      PIC X(30).
           05  WS-CUST-RANK      PIC X(01).
               88  WS-RANK-GOLD   VALUE 'G'.
               88  WS-RANK-SILVER VALUE 'S'.
           05  WS-CUST-BALANCE   PIC S9(13)V9(2) COMP-3.
           05  WS-CUST-POINTS    PIC S9(9)       COMP.
      *
      * 計算作業エリア
       01  WS-WORK.
           05  WS-TAX-RATE       PIC V9(2) VALUE .10.
           05  WS-TAX-AMOUNT     PIC S9(9)V9(2) COMP-3.
           05  WS-TOTAL-AMOUNT   PIC S9(11)V9(2) COMP-3.
           05  WS-DISCOUNT-RATE  PIC V9(2) VALUE .00.
      *
      * 処理結果
       01  WS-RESULT.
           05  WS-RETURN-CODE    PIC S9(4) COMP.
               88  WS-SUCCESS    VALUE ZERO.
               88  WS-NOT-FOUND  VALUE 100.
               88  WS-ERROR      VALUE 999.
           05  WS-ERROR-MSG      PIC X(80).
           05  WS-RESULT-AMOUNT  PIC S9(13)V9(2) COMP-3.
      *
      * SQL通信域
       01  SQLCA.
           05  SQLCODE           PIC S9(8) COMP.
      *
       PROCEDURE DIVISION.
      *----------------------------------------------------------------
      * メイン処理
      *----------------------------------------------------------------
       MAIN-PROCESS.
           PERFORM VALIDATE-INPUT
           IF WS-ERROR
               STOP RUN
           END-IF
      *
           PERFORM GET-CUSTOMER-INFO
           IF WS-NOT-FOUND
               MOVE 'Customer not found' TO WS-ERROR-MSG
               STOP RUN
           END-IF
      *
           IF WS-TYPE-UPDATE
               PERFORM CALCULATE-AMOUNT
               PERFORM UPDATE-CUSTOMER-BALANCE
           END-IF
      *
           PERFORM BUILD-RESULT
           STOP RUN.
      *
      *----------------------------------------------------------------
      * 入力バリデーション
      *----------------------------------------------------------------
       VALIDATE-INPUT.
           IF WS-CUSTOMER-ID = SPACES
               MOVE 999 TO WS-RETURN-CODE
               MOVE 'Customer ID is required' TO WS-ERROR-MSG
           END-IF
      *
           IF WS-AMOUNT < 0
               MOVE 999 TO WS-RETURN-CODE
               MOVE 'Amount must be positive' TO WS-ERROR-MSG
           END-IF.
      *
      *----------------------------------------------------------------
      * 顧客情報取得 (DB)
      *----------------------------------------------------------------
       GET-CUSTOMER-INFO.
           EXEC SQL
               SELECT CUST_NAME
                    , CUST_RANK
                    , CUST_BALANCE
                    , CUST_POINTS
               INTO  :WS-CUST-NAME
                    ,:WS-CUST-RANK
                    ,:WS-CUST-BALANCE
                    ,:WS-CUST-POINTS
               FROM  CUSTOMER_MASTER
               WHERE CUSTOMER_ID = :WS-CUSTOMER-ID
               FOR UPDATE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZERO
                   MOVE ZERO TO WS-RETURN-CODE
               WHEN 100
                   MOVE 100 TO WS-RETURN-CODE
               WHEN OTHER
                   MOVE 999 TO WS-RETURN-CODE
                   MOVE 'DB Error on SELECT' TO WS-ERROR-MSG
           END-EVALUATE.
      *
      *----------------------------------------------------------------
      * 金額計算（消費税・ランク割引）
      *----------------------------------------------------------------
       CALCULATE-AMOUNT.
      *    ランク別割引率の設定
           EVALUATE TRUE
               WHEN WS-RANK-GOLD
                   MOVE .05 TO WS-DISCOUNT-RATE
               WHEN WS-RANK-SILVER
                   MOVE .02 TO WS-DISCOUNT-RATE
               WHEN OTHER
                   MOVE .00 TO WS-DISCOUNT-RATE
           END-EVALUATE
      *
      *    割引後金額 = 入力金額 × (1 - 割引率)
           COMPUTE WS-TOTAL-AMOUNT =
               WS-AMOUNT * (1 - WS-DISCOUNT-RATE)
      *
      *    消費税加算
           COMPUTE WS-TAX-AMOUNT =
               WS-TOTAL-AMOUNT * WS-TAX-RATE
      *
           COMPUTE WS-TOTAL-AMOUNT =
               WS-TOTAL-AMOUNT + WS-TAX-AMOUNT.
      *
      *----------------------------------------------------------------
      * 顧客残高更新 (DB)
      *----------------------------------------------------------------
       UPDATE-CUSTOMER-BALANCE.
           COMPUTE WS-CUST-BALANCE =
               WS-CUST-BALANCE + WS-TOTAL-AMOUNT
      *
           EXEC SQL
               UPDATE CUSTOMER_MASTER
               SET    CUST_BALANCE = :WS-CUST-BALANCE
                    , UPDATED_AT  = CURRENT_TIMESTAMP
               WHERE  CUSTOMER_ID = :WS-CUSTOMER-ID
           END-EXEC
      *
           IF SQLCODE NOT = ZERO
               MOVE 999 TO WS-RETURN-CODE
               MOVE 'DB Error on UPDATE' TO WS-ERROR-MSG
               EXEC SQL ROLLBACK END-EXEC
           ELSE
               EXEC SQL COMMIT END-EXEC
           END-IF.
      *
      *----------------------------------------------------------------
      * 結果組み立て
      *----------------------------------------------------------------
       BUILD-RESULT.
           MOVE WS-TOTAL-AMOUNT TO WS-RESULT-AMOUNT
           IF WS-SUCCESS
               DISPLAY 'Processing completed for: ' WS-CUSTOMER-ID
           END-IF.
