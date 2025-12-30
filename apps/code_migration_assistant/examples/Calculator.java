package com.migration;

/**
 * Calculator - COBOL→Java移行.
 * 元のプログラム: CALCULATOR
 */
public class Calculator {

    // フィールド（COBOL変数から生成）
    private int num1;
    private int num2;
    private long result;
    private String operation;

    // メソッド（COBOLプロシージャから生成）
    public static void main(String[] args) {
        // TODO: COBOL PROCEDURE DIVISIONから移行
        // DISPLAY "CALCULATOR PROGRAM".
        // MOVE 100 TO WS-NUM1.
        // MOVE 200 TO WS-NUM2.
        // PERFORM ADD-NUMBERS.
        // DISPLAY "ADDITION RESULT: " WS-RESULT.
        // DISPLAY "SUBTRACTION RESULT: " WS-RESULT.
        // DISPLAY "MULTIPLICATION RESULT: " WS-RESULT.
        // ADD-NUMBERS.
        // ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
    }
}