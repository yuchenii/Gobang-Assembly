DATA SEGMENT
	BLACK EQU 1          ; 黑棋
	WHITE EQU 2          ; 白棋
	CHESSBOARD DB 218,13 DUP(194),191,13 DUP(195,13 DUP(197),180),192,13 DUP(193),217 	;设置棋盘的缓冲区, 1黑棋 2白棋
	X DB 0										;落子坐标 x
	Y DB 0                                       ;落子坐标 y
	MY DB 1										; 自己的棋子颜色，1黑棋 2白棋
	FLAG DB 0									;判断是否可以落子的标记，1为可以，0为不可以
	STATE DB 0									;目前的状态，单机：0为游戏进行中，2为一方退出；3为一方获胜
											;0该我下，1我已经下完，等待接受X；2等待接受Y；4对方获胜，5对方退出
	OVER DB 0									;判断是否比赛结束，CALL ISWIN 0为没有结束，1为结束。结束时，最后落子方获胜
	LED DB 3FH,06H,5BH,4FH,66H,6DH,7DH,07H,7FH,6FH     					;七段数码管对应显示
    S1 DB 0                             									;用于保存输入坐标值x
    S2 DB 0										;用于保存输入坐标值y
    TEMP DB 1                          	 								;判断该下黑子还是白子，0黑棋，1白棋，默认为1白棋先行
    ORDER DB 1                          								;双机时标志先手or后手，1表示先手，2表示后手
    MUSTYPE DB 1								;最终音乐类型
	SCORE DW 1                                  ; 当前位置的分数
	MAXSCORE DW 0                               ; 当前棋盘最高分数
	NUMM DW 0									; 已下棋子的个数
	TI DB ' 1 2 3 4 5 6 7 8 9 A B C D E F',0AH,0DH,'$'						;棋盘的y坐标
    ERROR DB 'YOU CANNOT PUT HERE!',0AH,0DH,'$' 						;报错,"你不能放在这里"
    WRONG DB 0AH,0DH,'FALSE INPUT!',0AH,0DH,'$'						;错误信息的提示
    COLOR DB 0AH,0DH,'PLEASE CHOOSE YOUR CHESSMAN COLOR:(1 FOR BLACK, 2 FOR WHITE)',0AH,0DH,'$'	;选棋子的颜色，1是黑色，2是白色
    CLEAN DB 72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,'$';更新棋盘
    PUT DB 'PLEASE INPUT THE POSITION(X Y):',0AH,0DH,'$'						;请输入棋子的位置(x,y)
    GAMEEND DB 'ONE PLAYER HAS WIN!',0AH,0DH,'$'						;游戏结束信息提示
    CONGRA DB 'YOU WIN! CONGRATULATIONS!',0AH,0DH,'$'					;游戏提示信息,"恭喜!你赢了!"
    SORRY DB 'YOU LOSE! DONNOT GIVE UP!',0AH,0DH,'$'						;游戏提示信息,"对不起,你输了,不要放弃!"
    WAIT1 DB 'PLEASE WAIT...',0AH,0DH,'$'  							;进入等待信息提示 
    CHOOSE DB 'PLEASE CHOOSE GAME MODEL:(1 FOR ONE PLAYER, 2 FOR TWO PLAYERS, ESC TO QUIT)',0AH,0DH,'$'	;选择游戏的玩法
    EXIT DB 'ONE PLAYER HAS QUIT!',0AH,0DH,'$'							;一个玩家退出后播放音乐
    MUS_FREQ DW 270,270,270,190,230,270,250,270,-1								;音乐播放结束符
    MUS_TIME DW 3 DUP (30),50,50,30,30,80 
	MUS_FREQ0 DW 230,150,-1
	MUS_TIME0 DW 30,30 
DATA ENDS
INISTACK SEGMENT STACK
	DW 128H DUP(0)									;初始化堆栈						
INISTACK ENDS

ADDRESS MACRO A,B
     LEA SI,A
     LEA BP,DS:B
ENDM

CODE SEGMENT
	ASSUME CS:CODE,DS:DATA,SS:INISTACK							;说明一个对应的关系，之后再把段的首地址赋值给段寄存器
START:
	MOV AX,DATA									;数据库装入段寄存器DS
	MOV DS,AX
    MOV AL,2
	MOV AH,0									;设置显示方式
	INT 10H
SELECT:                                 		;选择功能
    MOV DX,OFFSET CHOOSE                		;选择单机或双机游戏
	MOV AH,09H									;使用21H中断的设置光标位置功能
	INT 21H										;在屏幕上显示输入的内容
    MOV AH,1									;使用21H号中断的显示输入功能						
	INT 21H
	CMP AL,'1'                         			;1为单机
	JE GAME1									;输入为1，回到单机游戏
	CMP AL,'2'                          		;2为双机
	JZ MARK										;输入为1，进入积分
	CMP AL,27                           		;输入为ESC，退出
    JZ GEND0									;游戏结束
	MOV DX,OFFSET WRONG      					;输入不合规，要求重新输入
	MOV AH,09H									;在屏幕上显示内容
	INT 21H
	CALL BEEP									;调用报错音
	JMP SELECT									;回到选择功能
GEND0:
    MOV AH,4CH									;退出游戏
	INT 21H
;=========/*单机*/========
GAME1:	                               
	MOV AL,2									;在屏幕上显示输入的内容
	MOV AH,0
	INT 10H										;设置80*25黑白方式，清空屏幕
	CALL INITIAL								;初始化计数器
	CALL PRINT									;打印棋盘
	CALL SLED                           		;数码管显示当前状态
HERE1:
	MOV DX,OFFSET PUT							;放置棋子的提示语句
	MOV AH,09H									;在屏幕上显示输入的内容
	INT 21H
	MOV AH,1									;若输入的是ESC则退出
	INT 21H
	CMP AL,27									;若输入的是ESC
	JE QUIT										;退出游戏
	JMP RXY1									;否则输入坐标X Y
QUIT:											;退出游戏的信息
	MOV STATE,2									;把STATE的值设为2
	MOV DX,OFFSET EXIT							;有人退出就显示退出消息
	MOV AH,09H									;使用21H号中断的显示输入功能
	INT 21H 
	CALL SLED									;数码管显示当前状态
	JMP GEND1									;游戏结束
MARK:											;TODO待删
    JMP GAME2									;进行下一局游戏
RXY1:											;记录坐标X Y(ASCII码)
	MOV X,AL									;记录x的坐标
	INT 21H										;显示在屏幕上x的值
	CMP AL,27									;若是ESC则退出
	JE QUIT										;退出记录坐标x
	INT 21H										;显示在屏幕上y的值
	CMP AL,27									;若是ESC则退出
	JE QUIT										;退出记录坐标y
	MOV Y,AL									;记录y的坐标
N1:	MOV AH,07									;无回显输入
	INT 21H
	CMP AL,27									;若是ESC则退出
	JE QUIT										;退出游戏
	CMP AL,13									;若是回车则继续，否则等待回车
	JNE N1										;继续执行N1程序
	MOV AH,2
	MOV DL,0AH									;显示光标的行坐标
	INT 21H										;输出回车换行
	MOV DL,0DH									;显示光标的列坐标
	INT 21H										;输出回车换行
	MOV FLAG,1									;flag的值为1
	CALL CHECK									;检查可否落子，将X，Y改变为真实的数值
	CMP FLAG,1									;可以落子
	JE THERE1									;可以落子则判断落子后输赢
	JMP HERE1									;如果不可以落子则重新输入
THERE1:
	MOV MY,1									;我的坐标是1，对方的坐标是2
	CALL PUTDOWN1								;落子
	CALL ISWIN									;判断输赢，有结果则OVER=1
	CALL PRINT									;打印棋盘
	CMP OVER,1									;游戏结束																		
	JNZ HERE1
	MOV DX,OFFSET GAMEEND							;游戏结束的信息提示
    MOV AH,09H										;在屏幕上显示输入的内容
    INT 21H
    MOV AH,02H										;使用10H中断的设置位置功能																							
    MOV DL,00H										;设置光标的行坐标
    MOV DH,11H										;设置光标的列坐标
    INT 10H
    MOV STATE,3										;游戏结束我退出
    CALL SLED										;数码管显示当前状态
	MOV MUSTYPE, 1									;修改音乐类型
    CALL MUSIC										;播放音乐
GEND1:
	MOV AH,4CH									;退出游戏
	INT 21H
;=========/*人机*/========
GAME2:											; 人机游戏
    MOV DX,OFFSET COLOR               			;选择先后手，1后手，2先手
    MOV AH,09H									;在屏幕上显示输入的内容
    INT 21H
    MOV AH,1									;游戏开始
	INT 21H
	CMP AL,'1'
	JE SBLACK									;1为黑子
	CMP AL,'2'
	JZ SWHITE									;2为白子
	CMP AL,27									;若输入的是ESC
    JZ GEND1									;则退出
	MOV DX,OFFSET WRONG							;显示错误的提示信息
	MOV AH,09H									;在屏幕上显示输入的内容
	INT 21H
	CALL BEEP									;播放提示音
	JMP GAME2									;游戏结束
SBLACK:                               			;若执黑则后行，将我方棋子改为MY=1，ORDER改为2
    MOV MY,BLACK									; 我执黑棋，对方执白棋
    MOV STATE,1									;我已下完，正等待接收x
    MOV ORDER,2									;等待对方落子
SWHITE:											;若执白先行，将我方棋子改为白MY=2，ORDER改为1
	MOV AL,2  
	MOV AH,0
	INT 10H										;设置80*25黑白方式，清空屏幕
	CALL INITIAL								;初始化计数器和通信
	CALL PRINT									;打印棋盘
	CALL SLED									;数码管显示当前状态
HERE2:
	CMP STATE,0									;根据STATE输出提示信息
	JE SHOW										;应该是我下，跳转至SHOW
	MOV DX,OFFSET WAIT1							;应该是对方下，提示等待
	MOV AH,09H
	INT 21H										;输出请等待的信息
	MOV AH,02H
	MOV DL,00H									;光标从17,0开始
	MOV DH,11H									;光标的列坐标
	INT 10H
	CALL SLED									;数码管显示当前状态	
WW:	;TODO 这里发生了死循环
	CMP STATE,0									;根据STATE输出提示信息
	JE SHOW										;如果是我下，转至SHOW
	;TODO 缺少了STATE=1,即应该是对方下的情况
	CMP STATE,1	;                               ; STATE为1，则机器下
	JE ROBOT									;跳转至ROBOT,机器落子
	CMP STATE,4									;如果对方胜利
	JE ILOSE									;																		
	CMP STATE,5									;如果对方退出
	JE HQUIT									;
	JMP WW
ROBOT:
    MOV FLAG,1
    CALL FINDPLACE						        ; 机器寻找最优的落子位置
	CALL ROBOTPUTDOWN							; 机器落子
	CALL ISWIN                                  ; 判断是否赢了
	CALL PRINT									; 打印棋盘
	CMP OVER,1                                  ; 判断游戏是否结束
	JE ILOSE									; 机器胜利
	CALL BEEP									; 播放提示音
	MOV STATE,0									; 机器下完，改变STATE
	JMP HERE2                                   ; 机器下完我下
SHOW:
	MOV DX,OFFSET PUT							;提示落子信息
	MOV AH,09H									;在屏幕上显示输入的内容
	INT 21H
    CALL SLED									;数码管显示当前状态
	MOV AH,1									;若输入的是ESC则退出
	INT 21H
	CMP AL,27									;若输入的是ESC
	JE IQUIT									;我退出
	JMP RXY2									;否则输入坐标X Y
ILOSE:											;“我输了”的处理程序
	MOV DX,OFFSET SORRY							;提示抱歉信息
	MOV AH,09H									;在屏幕上显示输入的内容
	INT 21H
	CALL SLED									;数码管显示当前状态
	CALL MUSIC									;调用播放音乐
	JMP GEND2									;游戏结束信息提示
IQUIT:											;“我退出”的处理程序
	CALL SENDQ									;我退出就给对方发信息
	MOV STATE,3									;对方已获胜
HQUIT:											;“对方退出”的处理程序
	MOV DX,OFFSET EXIT							;有人退出就显示退出消息
	MOV AH,09H									;在屏幕上显示对方退出的信息
	INT 21H
	CALL SLED									;数码管显示当前状态
	JMP GEND2									;游戏结束信息提示
RXY2:											;记录坐标X Y(ASCII码)
	MOV X,AL									;显示x的坐标在屏幕上					
	INT 21H
	CMP AL,27									;若是ESC则退出
	JE IQUIT									;我退出
	INT 21H
	CMP AL,27									;若是ESC则退出
	JE IQUIT									;我退出
	MOV Y,AL									;显示y的坐标在屏幕上
N2:	MOV AH,07									;无回显输入
	INT 21H
	CMP AL,27									;如果是ESC
	JE IQUIT									;退出
	CMP AL,13									;如果是回车，继续；
	JNE N2										;如果不是回车，则循环等待
	MOV AH,2
	MOV DL,0AH									;输出回车换行
	INT 21H
	MOV DL,0DH									;光标的行坐标
	INT 21H										;输出回车换行
    mov AL,X                            			;保存输入的坐标值x用于以后发送
    mov S1,AL																								
    mov AL,Y										;保存输入的坐标值y用于以后发送
    mov S2,AL																															
	MOV FLAG,1									;可以落子
	CALL CHECK									;检查可否落子
	CMP FLAG,1									;flag=1，可以落子
	JE THERE2									;可以落子则判断落子
	JMP HERE2									;如果不可以落子则重新输入
THERE2:
    CMP ORDER,2										;选择后手落子
    JZ L2
L1:
 	MOV MY,WHITE									;我是白棋，对方是黑棋
 	JMP L3
L2: 
    MOV MY,BLACK								    ; 我是黑棋，对方是白棋
L3:
	CALL PUTDOWN2									;落子
	CALL ISWIN									;判断输赢，有结果则OVER=1
	CALL PRINT									;打印棋盘
    mov AL,S1																								
    mov X,AL										;保存输入的坐标值x用于以后发送
    mov AL,S2
    mov Y,AL        										;保存输入的坐标值y用于以后发送                                                
	CALL SEND									;并发送坐标
	CMP OVER,1									;判断我是否赢了
	JE IWIN										;跳转到IWIN子程序
	MOV STATE,1									;否则将STATE置1，表示我已下完，等待对方的X
	CALL SLED									;数码管显示当前状态
	JMP HERE2
IWIN:											;我赢了则显示祝贺信息并播放音乐
	MOV DX,OFFSET CONGRA								;祝贺信息显示
	MOV AH,09H									;在屏幕上显示输入的内容
	INT 21H
	MOV AH,02H
	MOV DL,00H									;光标从16,0开始
	MOV DH,10H									;光标的列坐标
	INT 10H										;屏幕上显示我胜利的信息
	MOV STATE,2									;我赢了
	CALL SLED									;数码管显示当前状态		
	MOV MUSTYPE, 1								;音乐类型为胜利音乐				
	CALL MUSIC									;播放音乐
GEND2:
	MOV AH,4CH									;退出游戏
	INT 21H
;=========/*数码管显示子程序*/========
SLED PROC NEAR										;数码管显示状态，0表示该我下，1表示等待对方下，2我赢了，3我退出，4对方赢了，5对方退出
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH DX
    MOV DX,0D413H
	MOV AL,80H									;设置8255方式0，A口输出
	OUT DX,AL
	MOV AL,STATE									;state的值
	MOV BX,OFFSET LED
	XLAT										;数码管根据STATE输出数字
    MOV DX,0D410H
	OUT DX,AL
	POP DX										;恢复CPU现场
	POP BX
	POP AX
	RET										;子程序结束返回
SLED ENDP
;=========/*初始化子程序*/========
INITIAL PROC NEAR										;计数器信息的提示
    MOV AX, CS
    MOV DS, AX
    MOV DX, OFFSET IRQ11                								; DS中断服务程序段地址、DX为偏移量
    MOV AX, 2573H                       								; AH=25H置中断向量
    INT 21H
    CLI
    MOV DX, 0D84CH                      								; PCI9052 中断状态、控制寄存器地址低位
    MOV AL, 43H                         								; 最低为1，开中断
    OUT DX, AL
    INC DX                              									; PCI9052 中断状态、控制寄存器地址高位
    MOV AL, 1DH                        					 			; 清除可能的中断状态
    OUT DX, AL        
    IN AL, 0A1H                         								; 从片                                                    
    AND AL, 11110111B                   								; 开放IRQ11中断
    OUT 0A1H,AL
    IN AL, 21H                          									;总片
    AND AL, 11111011B 
    OUT 21H, AL
    STI       
	MOV AL,00010110B	                						;初始化8253通道0，工作方式3，二进制
	MOV DX,0D403H
	OUT DX,AL
	MOV DX,0D400H
	MOV AL,52			               						;计数初值52
	OUT DX,AL			           
	MOV AX,DATA
	MOV DS,AX
	MOV DX,0D409H									;8251A控制口地址
	MOV AL,0
	OUT DX,AL
	OUT DX,AL
	OUT DX,AL
	MOV AL,40H									;写操作命令字，内部复位
	OUT DX,AL
	MOV AL,4EH									;方式字：异步，1位停止位，8位数据位，无奇偶校验，波特率16
	OUT DX,AL
	MOV AL,27H									;命令字：请求发送，运行发送和接收，数据终端准备好，发间断字符
	OUT DX,AL							
	RET										;子程序结束返回
INITIAL ENDP
;=========/*检验落子位置是否合法*/========
CHECK PROC NEAR										;落子位置是否合法的检查信息 
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
    MOV CX,2                                ; CMPD 循环两次
	MOV AL,X                                ; X 放入 AL
CMPD:	                                    ; 大小写转换，检查是否在棋盘范围内
	DEC CX                                  ; CX 减一
	CMP AL,'A'                              ; 判断 AL 是否小于 'A'
	JL CMPLA                                ; 如果小于，跳转到 CMPLA
	CMP AL,'F'								; 判断 AL 是否小于 'F'
	JL CMPLF                                ; 如果小于，跳转到 CMPLF
	CMP AL,'a'                              ; 判断 AL 是否小于 'a'
	JL MYERR                                ; 如果小于，报错
	CMP AL,'f'                              ; 判断 AL 是否大于 'f'
	JG MYERR                                ; 如果大于，报错
	SUB AL,32                               ; 小写转换为大写
	SUB AL,7                                ; 减去 '9' 与 'A' 的差值
CMPCX:	                                    ; 比较 CX,判断是否继续比较
	CMP CX,0                                ; 判断 CX 是否为 0
	JE SUBXY                                ; 如果等于零，Y已经检查过，跳转到 SUBXY
	MOV X,AL                                ; 将改变后的 AL 放入 X
	MOV AL,Y                                ; Y 放入 AL
	JMP CMPD                                ; 检查 Y
CMPLA:                                      ; AL 小于 'A',检查是否在0-9范围内
	CMP AL,'1'                              ; 判断 AL 是否小于 '1'
	JL MYERR                                ; 如果小于，报错
	CMP AL,'9'                              ; 判断 AL 是否大于 '9'
	JG MYERR                                ; 如果大于，报错
	JMP CMPCX                               ; 判断 Y 是否检查过
CMPLF:                                      ; AL 在 A-F 之间
	SUB AL,7                                ; 减去 '9' 与 'A' 的差值
	JMP CMPCX                               ; 判断 Y 是否检查过
SUBXY:
	MOV Y,AL                                ; 将改变后的 AL 放入 Y
    SUB X,'1'                            	; 将X改变为真实的值
	SUB Y,'1'								; 将Y改变为真实的值
	MOV CX,0								; 传送指令
	MOV CL,X
	MOV BX,0								; 清空寄存器
MULX1: 
    ADD BL,15										;棋子右移15单位
    LOOP MULX1										;循环MULX1
	ADD BL,Y										;棋子右移输入Y的值
	CMP CHESSBOARD[BX],BLACK                 			   ;若此处已有棋子，输入不合法
	JE MYERR							;
	CMP CHESSBOARD[BX],WHITE								;若此处没有棋子，输入合法
	JNE RETURNC 
MYERR:
    MOV FLAG,0                           		; 对于不合法的输入，显示错误信息，并鸣响扬声器
	MOV DX,OFFSET ERROR
	MOV AH,09H									;在屏幕上显示输入错误的信息
    INT 21H
	CALL BEEP									;调用报错音
RETURNC:
    POP DX										;恢复CPU现场
    POP CX
    POP BX
    POP AX
	RET										;子程序结束返回
CHECK ENDP
;=========/*单机落子子程序*/========
PUTDOWN1 PROC NEAR									;单机落子的信息提示					
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV CX,0									;字符指针初始化
	MOV CL,X
	MOV BX,0									;清空寄存器
MULX2: 
	ADD BL,15									;字符指针右移15个字节
	LOOP MULX2									;循环MULX2
	ADD BL,Y										;字符指针右移Y个字节
	CMP TEMP,0                         	 							;根据TEMP值，轮流放置黑子和白子
	JE MM1
	MOV CHESSBOARD[BX],WHITE								; 放白棋
	MOV TEMP,0									;根据TEMP值，轮流放置黑子和白子
	JMP YY1
MM1:	
    MOV CHESSBOARD[BX],BLACK									; 放黑棋
    MOV TEMP,1										;根据TEMP值，轮流放置黑子和白子
YY1:
    INC NUMM	                                    ; 棋子数量加1
    POP DX										;恢复CPU现场	
	POP CX
	POP BX
	POP AX
	RET										;子程序结束返回
PUTDOWN1 ENDP
;=========/*双机落子子程序*/========
PUTDOWN2 PROC NEAR									;双机落子的信息提示				
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV CX,0									;字符指针初始化
	MOV CL,X	
	MOV BX,0									;清空寄存器
MULX4: 
	ADD BL,15									;字符指针右移15个字节
	LOOP MULX4									;循环MULX4
	ADD BL,Y										;字符指针右移Y个字节
	CMP MY,1									; 判断我是白棋还是黑棋
	JE MM2
	MOV CHESSBOARD[BX],WHITE								;放置白棋
	MOV TEMP,0                                  ; 接下来轮到黑棋下 
	JMP YY2										;返回调用处
MM2:													
    MOV CHESSBOARD[BX],BLACK									;放置黑棋
	MOV TEMP,1                                  ; 接下来轮到白棋下
YY2:
    INC NUMM	                                    ; 棋子数量加1														 
	POP DX										;恢复CPU现场
	POP CX
	POP BX
	POP AX
	RET
PUTDOWN2 ENDP
;=====/*机器寻找落子的位置*/=======
FINDPLACE PROC NEAR
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV X,2
	MOV Y,2
	MOV SCORE,1                                 ;初始化分数
	MOV MAXSCORE,0							  ;初始化最大分数
	CMP NUMM,0                                 ; 判断是不是首颗棋子
	JNE FIND                                   ; 不是首颗棋子，跳转到FIND
	MOV BL,7                                   ; 是首颗棋子，则下正中间的位置
	MOV BH,7
	JMP HASFIND                                ; 已经找到落子位置，跳转到HASFIND
FIND:
    CMP MY,1
	MOV FLAG,1
	CALL CHECKPLACE 						    ; 调用检查子程序
	CMP FLAG,0                                  ; 如果不能落子
    JE NEXTY                                    ; 则继续寻找
	MOV SCORE,1                               ; 重置分数
	CALL CALSCORE							    ; 计算当前位置分数
	MOV CX,MAXSCORE
	CMP SCORE,CX						    
    JLE NEXTY									; 如果当前位置分数小于最高分数,则继续
	MOV CX,SCORE
	MOV MAXSCORE,CX								; 如果当前位置分数大于最高分数,则替换最高分数
	MOV BL,X                                 ; 保存当前最高分数的位置x
	MOV BH,Y                                  ; 保存当前最高分数的位置y
NEXTY:
    INC Y                                   ; 右移一位
	CMP Y,13
    JNE FIND
	MOV Y,2                                ; 从头开始
	INC X                                  ; 下面一行
	CMP X,13                               ; 判断是否已经到达最后一行
	JNE FIND                               ; 如果没有到达最后一行,则继续
HASFIND:
	MOV X,BL                               ; 保存最高分数的位置x
	MOV Y,BH                               ; 保存最高分数的位置y
    POP DX										;恢复CPU现场
	POP CX
	POP BX
	POP AX
	RET
FINDPLACE ENDP	 
;=====/*检查机器落子位置是否合法*/=======
CHECKPLACE PROC NEAR
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	CMP X,0
	JL INPUTERR
	CMP X,14
	JG INPUTERR
	CMP Y,0
	JL INPUTERR
	CMP Y,14
	JG INPUTERR
	MOV CX,0								; 传送指令
	MOV CL,X
	MOV BX,0								; 清空寄存器
MULX5: 
    ADD BL,15										;棋子右移15单位
    LOOP MULX5										;循环MULX1
	ADD BL,Y										;棋子右移输入Y的值
	CMP CHESSBOARD[BX],BLACK                 			   ;若此处已有棋子，输入不合法
	JE INPUTERR							;
	CMP CHESSBOARD[BX],WHITE								;若此处没有棋子，输入合法
	JNE FINISHCHECK
INPUTERR:
    MOV FLAG,0                           		; 对于不合法的输入，显示错误信息，并鸣响扬声器
	MOV DX,OFFSET WAIT1							;应该是对方下，提示等待
	MOV AH,09H
	INT 21H										;输出请等待的信息
FINISHCHECK:
    POP DX										;恢复CPU现场										
	POP CX                                    
	POP BX
	POP AX
	RET
CHECKPLACE ENDP
;=====/*计算当前位置分数*/=======
CALSCORE PROC NEAR
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV DX,0
    MOV BX,0
	MOV CX,0
	MOV CL,X
	CMP MY,1        ; 判断机器黑棋还是白棋
    JE MYBLACK
	MOV DL,BLACK    ; 机器是黑棋
	MOV DH,WHITE    ; 玩家是白棋
    JMP MULX7
MYBLACK:
    MOV DL,WHITE    ; 机器是白棋
	MOV DH,BLACK    ; 玩家是黑棋
    JMP MULX7
MULX7:
    ADD BL,15
	LOOP MULX7										
	ADD BL,Y
	CALL SCORE1
	CMP SCORE,10000
	JGE FINISHCAL
	CALL SCORE2
	CMP SCORE,10000 
	JGE FINISHCAL
	CALL SCORE3
	CMP SCORE,10000
	JGE FINISHCAL
	CALL SCORE4
	CMP SCORE,10000
	JMP FINISHCAL
FINISHCAL:
    POP DX
	POP CX
	POP BX
	POP AX
	RET
CALSCORE ENDP
;=========/*判断横向是否连成5个*/========
SCORE1 PROC NEAR								;横向判断子程序
    PUSH BX										;保存cpu现场
    CMP Y,10	 									;判断横向是否有10个字节
    JG  SC10										;若小于则横向不能连成5个
    CMP DL,CHESSBOARD[BX+1]								;判断棋盘横向是否有2个棋子连在一起
    JNE SC11
    CMP DL,CHESSBOARD[BX+2]								;判断棋盘横向是否有3个棋子连在一起
    JNE SC12 
    CMP DL,CHESSBOARD[BX+3]								;判断棋盘横向是否有4个棋子连在一起
    JNE SC13
    CMP DL,CHESSBOARD[BX+4]								;判断棋盘横向是否有5个棋子连在一起
    JNE SC14
    ADD SCORE,10000										;1111,10000分
	JMP SC10
SC11:
    CMP DH,CHESSBOARD[BX+1]
	JE SC111
	ADD SCORE,2                          ; 0XXX,2分  
	JMP SC10
SC111:
	ADD SCORE,3                          ; 2XXX，3分   
	JMP SC10
SC12:
    CMP DH,CHESSBOARD[BX+2]
	JE SC121
	ADD SCORE,10                          ; 10XX,10分
	JMP SC10
SC121:
	ADD SCORE,5                          ; 12XX,5分
	JMP SC10
SC13:
	CMP DH,CHESSBOARD[BX+3]
	JE SC131
	ADD SCORE,200                       ; 110X,200分
	JMP SC10
SC131:
    ADD SCORE,8                            ; 112X,8分
	JMP SC10	
SC14:
	CMP DH,CHESSBOARD[BX+4]
	JE SC141
	ADD SCORE,1000                        ; 1110,1000分
	JMP SC10
SC141:
	ADD SCORE,100                           ; 1112,100分
SC10: 
    POP BX										;恢复cpu现场
    RET											;子程序结束返回 
SCORE1 ENDP
;=========/*判断纵向是否连成5个*/========
SCORE2 PROC NEAR										;纵向判断子程序
   PUSH BX										;保存cpu现场
   CMP X,10										;判断纵向是否有10个字节
   JG SC20										;若小于则纵向不能连成5个
   CMP DL,CHESSBOARD[BX+15]								;判断棋盘纵向是否有2个棋子连在一起
   JNE SC21
   CMP DL,CHESSBOARD[BX+30]								;判断棋盘纵向是否有3个棋子连在一起
   JNE SC22
   CMP DL,CHESSBOARD[BX+45]								;判断棋盘纵向是否有4个棋子连在一起
   JNE SC23
   CMP DL,CHESSBOARD[BX+60]								;判断棋盘纵向是否有5个棋子连在一起
   JNE SC24
   ADD SCORE,10000   									;1111,10000分
SC21:
   CMP DH,CHESSBOARD[BX+15]
   JE SC211
   ADD SCORE,2                          ; 0XXX,2分  
   JMP SC20	
SC211:
	ADD SCORE,3                          ; 2XXX，3分   
    JMP SC20
SC22:
   	CMP DH,CHESSBOARD[BX+30]
   	JE SC221
   	ADD SCORE,10                          ; 10XX,10分
	JMP SC20
SC221:
	ADD SCORE,5                          ; 12XX,5分
    JMP SC20
SC23:
	CMP DH,CHESSBOARD[BX+45]
	JE SC231
	ADD SCORE,200                       ; 110X,200分
	JMP SC20
SC231:
	ADD SCORE,8                            ; 112X,8分
	JMP SC20
SC24:
	CMP DH,CHESSBOARD[BX+60]
	JE SC241
	ADD SCORE,1000                        ; 1110,1000分
	JMP SC20
SC241:
	ADD SCORE,100                           ; 1112,100分
SC20: 
   POP BX
   RET											;子程序结束返回
SCORE2 ENDP
;=========/*判断斜上是否连成5个*/========
SCORE3 PROC NEAR										;斜上判断子程序
   PUSH BX										;保存cpu现场
   CMP X,4		      								;判断纵向是否有4个字节                  																	
   JL SC30										;若小于则斜上不能连成5个
   CMP Y,10										;判断横向是否有10个字节
   JG SC30
   CMP DL,CHESSBOARD[BX-14]								;判断棋盘斜上是否有2个棋子连在一起
   JNE SC31
   CMP DL,CHESSBOARD[BX-28]								;判断棋盘斜上是否有3个棋子连在一起
   JNE SC32
   CMP DL,CHESSBOARD[BX-42]								;判断棋盘斜上是否有4个棋子连在一起
   JNE SC33
   CMP DL,CHESSBOARD[BX-56]								;判断棋盘斜上是否有5个棋子连在一起
   JNE SC34
   MOV SCORE,10000   									;1111,10000分
SC31:
	CMP DH,CHESSBOARD[BX-14]
   	JE SC311
   	ADD SCORE,2                          ; 0XXX,2分  
   	JMP SC30
SC311:
	ADD SCORE,3                          ; 2XXX，3分   
	JMP SC30
SC32:
   	CMP DH,CHESSBOARD[BX-28]
   	JE SC321
   	ADD SCORE,10                          ; 10XX,10分
	JMP SC30
SC321:
	ADD SCORE,5                          ; 12XX,5分
	JMP SC30
SC33:
	CMP DH,CHESSBOARD[BX-42]
	JE SC331
	ADD SCORE,200                       ; 110X,200分
	JMP SC30
SC331:
	ADD SCORE,8                            ; 112X,8分
	JMP SC30
SC34:
	CMP DH,CHESSBOARD[BX-56]
	JE SC341
	ADD SCORE,1000                        ; 1110,1000分
	JMP SC30
SC341:
	ADD SCORE,100                           ; 1112,100分
SC30: 
   POP BX
   RET											;子程序结束返回
SCORE3 ENDP
;=========/*判断斜下是否连成5个*/========
SCORE4 PROC NEAR										;斜下判断子程序
   PUSH BX										;保存cpu现场
   CMP X,10										;判断纵向是否有10个字节
   JG SC40										;若小于则斜下不能连成5个
   CMP Y,10										;判断横向是否有10个字节
   JG SC40         									;若小于则斜下不能连成5个  ;不能斜下
   CMP DL,CHESSBOARD[BX+16]								;判断棋盘斜下是否有2个棋子连在一起 
   JNE SC41
   CMP DL,CHESSBOARD[BX+32]								;判断棋盘斜下是否有3个棋子连在一起
   JNE SC42
   CMP DL,CHESSBOARD[BX+48]								;判断棋盘斜下是否有4个棋子连在一起
   JNE SC43
   CMP DL,CHESSBOARD[BX+64]								;判断棋盘斜下是否有5个棋子连在一起
	JNE SC44
	MOV SCORE,10000   									;1111,10000分
SC41:
	CMP DH,CHESSBOARD[BX+16]
   	JE SC411
   	ADD SCORE,2                          ; 0XXX,2分  
   	JMP SC40
SC411:
	ADD SCORE,3                          ; 2XXX，3分   
	JMP SC40
SC42:
   	CMP DH,CHESSBOARD[BX+32]
   	JE SC421
   	ADD SCORE,10                          ; 10XX,10分
SC421:
	ADD SCORE,5                          ; 12XX,5分
	JMP SC40
SC43:
	CMP DH,CHESSBOARD[BX+48]
	JE SC431
	ADD SCORE,200                       ; 110X,200分
	JMP SC40
SC431:
	ADD SCORE,8                            ; 112X,8分
	JMP SC40
SC44:
    CMP DH,CHESSBOARD[BX+64]
	JE SC441
	ADD SCORE,1000                        ; 1110,1000分
SC441:
	ADD SCORE,100                           ; 1112,100分																		
SC40: 
   POP BX
   RET											;子程序结束返回
SCORE4 ENDP 	 


;========/*机器落子*/=============
ROBOTPUTDOWN PROC NEAR
	PUSH AX										;保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV CX,0									;字符指针初始化
	MOV CL,X	
	MOV BX,0									;清空寄存器
MULX6: 
	ADD BL,15									;字符指针右移15个字节
	LOOP MULX6									;循环MULX4
	ADD BL,Y									;字符指针右移Y个字节
	CMP MY,2									; 判断我是白棋还是黑棋
	JE MM3                                      ; 如果是白棋,则机器为黑棋
	MOV CHESSBOARD[BX],WHITE					; 放置白棋
	MOV TEMP,0                                   ; 接下来轮到黑棋下
	JMP HASPUTDOWN								; 找到了落子位置
MM3:													
    MOV CHESSBOARD[BX],BLACK					; 放置黑棋
	MOV TEMP,1                                   ; 接下来轮到白棋下
HASPUTDOWN:
    INC NUMM	                                    ; 棋子数量加1
	POP DX										;恢复CPU现场
	POP CX
	POP BX
	POP AX
	RET
ROBOTPUTDOWN ENDP
;=========/*判断是否获胜*/========
ISWIN PROC NEAR										;我获胜的信息提示
    MOV X,0										;初始化X和Y
    MOV Y,0
LOOPY:
    MOV CX,0										;字符指针初始化
	MOV CL,X
	MOV BX,0									;清空寄存器
MULX3: 
    ADD BL,15										;字符指针右移15个字节
	LOOP MULX3									;循环MULX3
	ADD BL,Y                           								;BX=15*X+Y
	MOV DL,CHESSBOARD[BX]               																	
	CMP TEMP,0                         		   ; TEMP=0,判断白子是否获胜
	JZ L4
	CMP DL,BLACK										;判断黑子是否获胜
	JE PANDUAN									;判断黑子是否可以连成5个
	JMP NEXT									;进入下一轮判断
L4:
	CMP DL,WHITE										;判断白子是否获胜
    JE PANDUAN										;判断白子是否连成5个
	JMP NEXT 									;进入下一轮判断
PANDUAN: 										;游戏胜利的判断
    CALL TEST1                          								;横着
	CMP OVER,1									;横着连成5个游戏结束
	JE RETURNISWIN									;返回胜利的判断
	CALL TEST2                         								;竖着
    CMP OVER,1										;竖着连成5个游戏结束
	JE RETURNISWIN									;返回胜利的判断
	CALL TEST3                          								;斜上
	CMP OVER,1									;斜上连成5个游戏结束
	JE RETURNISWIN									;返回胜利的判断
	CALL TEST4                          								;斜下
    CMP OVER,1										;斜下连成5个游戏结束
	JE RETURNISWIN									;返回胜利的判断
NEXT: 
    INC Y											;Y的字符指针右移
	CMP Y,15										;比较Y的值
	JNE LOOPY
	MOV Y,0										;初始化Y的值
	INC X										;X的字符指针右移
	CMP X,15										;比较X的值
	JNE LOOPY
RETURNISWIN:
    RET											;子程序结束返回
ISWIN ENDP
;=========/*判断横向是否连成5个*/========
TEST1 PROC NEAR										;横向判断子程序
    PUSH BX										;保存cpu现场
    CMP Y,10	 									;判断横向是否有10个字节
    JG  RETURN1										;若小于则横向不能连成5个
    CMP DL,CHESSBOARD[BX+1]								;判断棋盘横向是否有2个棋子连在一起
    JNE RETURN1
    CMP DL,CHESSBOARD[BX+2]								;判断棋盘横向是否有3个棋子连在一起
    JNE RETURN1 
    CMP DL,CHESSBOARD[BX+3]								;判断棋盘横向是否有4个棋子连在一起
    JNE RETURN1
    CMP DL,CHESSBOARD[BX+4]								;判断棋盘横向是否有5个棋子连在一起
    JNE RETURN1
    MOV OVER,1										;游戏结束
RETURN1: 
    POP BX										;恢复cpu现场
    RET											;子程序结束返回 
TEST1 ENDP
;=========/*判断纵向是否连成5个*/========
TEST2 PROC NEAR										;纵向判断子程序
   PUSH BX										;保存cpu现场
   CMP X,10										;判断纵向是否有10个字节
   JG RETURN2										;若小于则纵向不能连成5个
   CMP DL,CHESSBOARD[BX+15]								;判断棋盘纵向是否有2个棋子连在一起
   JNE RETURN2
   CMP DL,CHESSBOARD[BX+30]								;判断棋盘纵向是否有3个棋子连在一起
   JNE RETURN2
   CMP DL,CHESSBOARD[BX+45]								;判断棋盘纵向是否有4个棋子连在一起
   JNE RETURN2
   CMP DL,CHESSBOARD[BX+60]								;判断棋盘纵向是否有5个棋子连在一起
   JNE RETURN2
   MOV OVER,1   										;游戏结束
RETURN2: 
   POP BX
   RET											;子程序结束返回
TEST2 ENDP
;=========/*判断斜上是否连成5个*/========
TEST3 PROC NEAR										;斜上判断子程序
   PUSH BX										;保存cpu现场
   CMP X,4		      								;判断纵向是否有4个字节                  																	
   JL RETURN3										;若小于则斜上不能连成5个
   CMP Y,10										;判断横向是否有10个字节
   JG RETURN3
   CMP DL,CHESSBOARD[BX-14]								;判断棋盘斜上是否有2个棋子连在一起
   JNE RETURN3
   CMP DL,CHESSBOARD[BX-28]								;判断棋盘斜上是否有3个棋子连在一起
   JNE RETURN3
   CMP DL,CHESSBOARD[BX-42]								;判断棋盘斜上是否有4个棋子连在一起
   JNE RETURN3
   CMP DL,CHESSBOARD[BX-56]								;判断棋盘斜上是否有5个棋子连在一起
   JNE RETURN3
   MOV OVER,1   										;游戏结束
RETURN3: 
   POP BX
   RET											;子程序结束返回
TEST3 ENDP
;=========/*判断斜下是否连成5个*/========
TEST4 PROC NEAR										;斜下判断子程序
   PUSH BX										;保存cpu现场
   CMP X,10										;判断纵向是否有10个字节
   JG RETURN4										;若小于则斜下不能连成5个
   CMP Y,10										;判断横向是否有10个字节
   JG RETURN4         									;若小于则斜下不能连成5个  ;不能斜下
   CMP DL,CHESSBOARD[BX+16]								;判断棋盘斜下是否有2个棋子连在一起 
   JNE RETURN4
   CMP DL,CHESSBOARD[BX+32]								;判断棋盘斜下是否有3个棋子连在一起
   JNE RETURN4
   CMP DL,CHESSBOARD[BX+48]								;判断棋盘斜下是否有4个棋子连在一起
   JNE RETURN4
   CMP DL,CHESSBOARD[BX+64]								;判断棋盘斜下是否有5个棋子连在一起
	JNE RETURN4
	MOV OVER,1   									;游戏结束																		
RETURN4: 
   POP BX
   RET											;子程序结束返回
TEST4 ENDP 	 
;=========/*打印棋盘*/========
PRINT PROC NEAR										;打印棋盘
	PUSH SI
	PUSH AX										;保存CPU现场
	PUSH DX
	MOV AH,02H									;使用10H中断的设置光标位置功能
	MOV DL,00H									;光标从0,0开始
    MOV DH,00H										;光标的列坐标
    INT 10H	
    MOV DX,OFFSET TI									;指定字符串  
    MOV AH,09H										;屏幕显示字符串
    INT 21H
	MOV X,0										;初始化X Y SI
	MOV Y,0
	MOV SI,0
LOOP2: 
    CMP Y,0										;判断Y是否为0
    JNE NOTHEAD
    MOV DL,X
    ADD DL,31H										;X的字符指针右移
	CMP DL,'9'									;判断X是否大于等于9
	JLE PP
	ADD DL,39									;X的字符指针右移39个字节 
PP:
    MOV AH,02H
    INT 21H										;使用21H中断的输出字符功能
NOTHEAD:
    MOV DL,CHESSBOARD[SI]
    MOV AH,02H
	INT 21H
	INC SI										;SI、Y指针同时右移1个字节，指向下一个字符
	INC Y										;SI、Y指针同时右移1个字节，指向下一个字符
	CMP Y,15										;判断Y的大小
	JE NEXTLINE
	MOV DL,'-'									;输出一个'-'
	MOV AH,02H									;使用21H中断的输出字符功能
	INT 21H
	JMP LOOP2									;回到循环2
NEXTLINE:
    MOV DL,32
    MOV AH,02H
	INT 21H
	MOV DL,0AH									;输出一个回车符（0AH）
	MOV AH,02H									;使用21H中断的输出字符功能
	INT 21H
	MOV DL,0DH									;输出一个换行符（0AD）
	MOV AH,02H									;使用21H中断的输出字符功能
	INT 21H
    INC X											;X的字符指针右移1个字节
	MOV Y,0										;初始化Y
    CMP X,15
	JNE LOOP2
    MOV DX,OFFSET CLEAN								;更新屏幕的信息提示
    MOV AH,09H										;使用21H中断的显示字符串功能
    INT 21H
    MOV AH,02H										;使用10H中断的设置光标位置功能
	MOV DL,00H									;光标从0,17开始
    MOV DH,10H										;设置光标的列坐标
	INT 10H
	POP DX										;恢复CPU现场
	POP AX
	POP SI
	RET										;子程序结束返回
PRINT ENDP 
;=========/*鸣响扬声器*/========
BEEP PROC NEAR										;鸣响扬声器子程序
		MOV MUSTYPE, 0						;修改音乐类型为警告
		CALL MUSIC
		RET
BEEP ENDP
;=========/*播放音乐*/========
GENSOUND PROC NEAR
     PUSH AX            ;保存CPU现场
     PUSH BX
     PUSH CX
     PUSH DX
     PUSH DI

     MOV AL, 0B6H
     OUT 43H, AL
     MOV DX, 12H
     MOV AX, 348CH
     DIV DI
     OUT 42H, AL

     MOV AL, AH
     OUT 42H, AL

     IN AL, 61H
     MOV AH, AL
     OR AL, 3
     OUT 61H, AL
SNDWAIT:
     MOV CX, 3314
     CALL WAITF
DELAY1:
     DEC BX
     JNZ SNDWAIT

     MOV AL, AH
     OUT 61H, AL

     POP DI            ;恢复CPU现场
     POP DX
     POP CX
     POP BX
     POP AX
     RET 
GENSOUND ENDP

;--------------------------
WAITF PROC NEAR
	PUSH AX            ;保存CPU现场
WAITF1:
      IN AL,61H
      AND AL,10H
      CMP AL,AH
      JE WAITF1
      MOV AH,AL
      LOOP WAITF1
      POP AX
      RET
WAITF ENDP
;--------------发声调用函数----------------
MUSIC PROC NEAR
    MOV AX, DATA
    MOV DS, AX
    ;MOV AX, INISTACK
    ;MOV SS, AX
    ;MOV SP, 300
	CMP MUSTYPE,1						;判断音乐类型
	JE MUS_T1
	ADDRESS MUS_FREQ0, MUS_TIME0		;取音乐0的地址
	JMP MUSSTT
MUS_T1:
	ADDRESS MUS_FREQ, MUS_TIME			;取音乐1的地址
MUSSTT:									;初始化完成，开始播放工作
    XOR AX, AX
FREG:
      MOV DI, [SI]
      CMP DI, 0FFFFH
      JE END_MUS
      MOV BX, DS:[BP]
      CALL GENSOUND
      ADD SI, 2
      ADD BP, 2
      JMP FREG
END_MUS:
    RET
MUSIC ENDP

;=========/*发送X Y*/========
SEND PROC NEAR										;查询方式发送X Y OVER
	PUSH AX											;保存CPU现场
	PUSH DX
LOOPX:
	MOV DX,0D409H
	IN AL,DX
	TEST AL,1
	JZ LOOPX
	MOV DX,0D408H
	MOV AL,X
	OUT DX,AL
MLOOP: 
    MOV DX,0D409H
    IN AL,DX
    TEST AL,1
    JZ MLOOP
    MOV DX,0D408H
    MOV AL,Y
    OUT DX,AL
	POP DX										;恢复CPU现场
	POP AX
	RET										;子程序结束返回
SEND ENDP
;/*发送我退出的消息*/
SENDQ PROC NEAR																															
	PUSH AX										;保存CPU现场
	PUSH DX
LOOY:
	MOV DX,0D409H
	IN AL,DX
	TEST AL,1
	JZ LOOY
	MOV DX,0D408H
	MOV AL,59									;我退出，发59(避开有效输入字符)
    OUT DX,AL
	POP DX										;恢复CPU现场
	POP AX
	RET										;子程序结束返回
SENDQ ENDP
;=========/*发送获胜消息*/========
SENDW PROC NEAR									;发送我退出的消息
	PUSH AX										;保存CPU现场
	PUSH DX
LOY:
	MOV DX,0D409H
	IN AL,DX
	TEST AL,1
	JZ LOY
	MOV DX,0D408H
	MOV AL,60									;我获胜，发60
    OUT DX,AL
	POP DX										;恢复CPU现场
	POP AX
	RET										;子程序结束返回
SENDW ENDP
;=========/*中断子程序*/========
IRQ11 PROC FAR					    
	PUSH AX										;保存CPU现场
	PUSH DX
    PUSH CX
	MOV DX,0D408H
	IN AL,DX
    CMP AL,59                           			
    JNZ L9											;若对方退出，将状态改为5
    MOV STATE,5										;对方退出
    CALL SLED										;数码管显示当前状态5
    JMP CLE										;清空棋盘
L9:    
    CMP AL,60
    jz hwin																				
    CMP STATE,1                         								;STATE=1表示我已下完，准备接收X
    JE XX											;准备接收x
    CMP STATE,2                        			 					;STATE=2表示准备接收Y
    JE YY											;准备接收y
    JMP CLE										;更新棋盘
XX: 											;接收X并将STATE=2
    MOV X,AL										;接收x的坐标
    MOV STATE,2
    JMP CLE										;在棋盘上显示坐标
YY:									
    MOV Y,AL										;接收y
    CMP ORDER,2										;该他下子
    JZ L7
    MOV MY,2										;我的坐标是2
    call check										;检查落子是否合法
    CALL PUTDOWN2									;双机落子
    CALL PRINT										;打印棋盘
    MOV MY,1										;我的坐标是1
    mov state,0										;游戏正在进行中
    call sled      										;数码管显示当前状态                                             
    JMP CLE										;棋盘屏幕更新
L7: 
    MOV MY,1										;我的坐标是1，对方的坐标是2
    call check										;检查落子是否合法
    CALL PUTDOWN2									;落子
    CALL PRINT										;打印棋盘
    MOV MY,2										;我的坐标是2，对方坐标是1
    mov state,0										;该我落子
    call sled   										;数码管显示当前状态                                                 
    JMP CLE										;屏幕棋盘更新
HWIN:											;如果他赢了，STATE=4
    MOV STATE,4										;对方赢了
    CALL SLED    										;数码管显示当前状态                                                  
    JMP CLE										;清空棋盘
CLE:
	MOV AL,20H									;清除PCI9052中断标志
	OUT 20H,AL
    mov al,20h
    out 0a0h,al 
	MOV DX,0D84DH
	MOV AL,1DH
	OUT DX,AL									;清9052中断标志
    POP CX
	POP DX										;恢复CPU现场
	POP AX
	IRET										;中断返回
IRQ11 ENDP
CODE ENDS
	END START	
