       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MANAGER.
       AUTHOR. SYSTEM.
       DATE-WRITTEN. 2025-11-20.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-EMPLOYEE-RECORD.
          05 WS-EMP-ID PIC 9(6).
          05 WS-EMP-NAME PIC X(30).
          05 WS-EMP-DEPT PIC X(20).
          05 WS-EMP-SALARY PIC 9(8)V99.
          05 WS-EMP-HIRE-DATE.
             10 WS-HIRE-YEAR PIC 9(4).
             10 WS-HIRE-MONTH PIC 9(2).
             10 WS-HIRE-DAY PIC 9(2).
       
       01 WS-COUNTERS.
          05 WS-TOTAL-EMPLOYEES PIC 9(5) VALUE ZERO.
          05 WS-TOTAL-SALARY PIC 9(12)V99 VALUE ZERO.
          05 WS-AVG-SALARY PIC 9(8)V99 VALUE ZERO.
       
       01 WS-FLAGS.
          05 WS-EOF-FLAG PIC X VALUE 'N'.
             88 WS-EOF VALUE 'Y'.
          05 WS-VALID-FLAG PIC X VALUE 'Y'.
             88 WS-VALID VALUE 'Y'.
             88 WS-INVALID VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "EMPLOYEE MANAGEMENT SYSTEM".
           DISPLAY "==========================".
           
           PERFORM INITIALIZE-SYSTEM.
           PERFORM PROCESS-EMPLOYEES UNTIL WS-EOF.
           PERFORM CALCULATE-STATISTICS.
           PERFORM DISPLAY-RESULTS.
           
           STOP RUN.
       
       INITIALIZE-SYSTEM.
           MOVE ZERO TO WS-TOTAL-EMPLOYEES.
           MOVE ZERO TO WS-TOTAL-SALARY.
           MOVE ZERO TO WS-AVG-SALARY.
           MOVE 'N' TO WS-EOF-FLAG.
           DISPLAY "System initialized.".
       
       PROCESS-EMPLOYEES.
           PERFORM READ-EMPLOYEE.
           IF NOT WS-EOF
              PERFORM VALIDATE-EMPLOYEE
              IF WS-VALID
                 PERFORM ADD-TO-STATISTICS
              END-IF
           END-IF.
       
       READ-EMPLOYEE.
           MOVE 123456 TO WS-EMP-ID.
           MOVE "JOHN DOE" TO WS-EMP-NAME.
           MOVE "IT DEPARTMENT" TO WS-EMP-DEPT.
           MOVE 75000.00 TO WS-EMP-SALARY.
           MOVE 2020 TO WS-HIRE-YEAR.
           MOVE 01 TO WS-HIRE-MONTH.
           MOVE 15 TO WS-HIRE-DAY.
           MOVE 'Y' TO WS-EOF-FLAG.
       
       VALIDATE-EMPLOYEE.
           IF WS-EMP-ID > 0 AND WS-EMP-SALARY > 0
              MOVE 'Y' TO WS-VALID-FLAG
           ELSE
              MOVE 'N' TO WS-VALID-FLAG
              DISPLAY "Invalid employee record: " WS-EMP-ID
           END-IF.
       
       ADD-TO-STATISTICS.
           ADD 1 TO WS-TOTAL-EMPLOYEES.
           ADD WS-EMP-SALARY TO WS-TOTAL-SALARY.
           DISPLAY "Processed employee: " WS-EMP-NAME.
       
       CALCULATE-STATISTICS.
           IF WS-TOTAL-EMPLOYEES > 0
              DIVIDE WS-TOTAL-SALARY BY WS-TOTAL-EMPLOYEES
                 GIVING WS-AVG-SALARY
              END-DIVIDE
           END-IF.
       
       DISPLAY-RESULTS.
           DISPLAY "==========================".
           DISPLAY "STATISTICS:".
           DISPLAY "Total Employees: " WS-TOTAL-EMPLOYEES.
           DISPLAY "Total Salary: " WS-TOTAL-SALARY.
           DISPLAY "Average Salary: " WS-AVG-SALARY.
           DISPLAY "==========================".

