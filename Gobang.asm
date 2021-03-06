DATA SEGMENT
	BLACK EQU 1         							  ; 黑棋
	WHITE EQU 2          							  ; 白棋
	X DB 0										      ; 落子坐标 x
	Y DB 0                                            ; 落子坐标 y
	MY DB 1										      ; 自己的棋子颜色, 1黑棋 2白棋
	STATE DB 0									      ; 目前的状态, 双人:0为游戏进行中, 2为一方退出；3为一方获胜
											          ; 人机:0该我下, 1我已经下完，对方下；4对方获胜, 5对方退出
    ORDER DB 1                          		      ; 人机时标志先手or后手, 1表示先手, 2表示后手
	SCORE DW 1                                        ; 当前位置的分数
	MAXSCORE1 DW 0                                    ; 第一层网络最高分数
	MAXSCORE2 DW 0                                    ; 第二层网络最高分数
	X1 DB 0                                           ; 第一层网络最高分数的坐标x
	Y1 DB 0                                           ; 第一层网络最高分数的坐标y
	CB1 DB 197  								      ; X1,Y1对应的棋盘缓冲区的值
	CHESSMODEL DB 9 DUP(0)						      ; 9种棋形2XXX,0XXX,12XX,10XX,112X,110X,1112,1110,1111对应的个数
	CHESSSCORE DW 3,2,5,10,8,200,200,1000,10000       ; 9种棋形2XXX,0XXX,12XX,10XX,112X,110X,1112,1110,1111对应的分数
	CHESSBOARD DB 218,13 DUP(194),191,13 DUP(195,13 DUP(197),180),192,13 DUP(193),217 	; 存储棋盘的信息, 同时可用于打印; 1黑棋 2白棋; 其余为构成棋盘的字符
	CLEANBOARD DB 72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,72 DUP(32),0AH,0DH,'$'; 空白棋盘
	FLAG_PUTDOWN DB 0								  					; 判断是否可以落子的标记, 1为可以, 0为不可以
	FLAG_NEXTSTEP DB 1                          	  					; 判断该下黑子还是白子, 0黑棋, 1白棋, 默认为1白棋先行
	FLAG_NUM DW 0									      				; 已下棋子的个数
	FLAG_OVER DB 0									  					; 判断是否比赛结束, 0为没有结束, 1为结束。默认为0，结束时, 最后落子方获胜
	FLAG_TRANS DB 0									  					; 用于判断代码是否要中转(套娃跳转), 默认不需要
	FIRSTLINE DB ' 1 2 3 4 5 6 7 8 9 A B C D E F','$' 					; 棋盘的y坐标
	STR_ERROR DB 'You Cannot Put Here!',0AH,0DH,'$' 	  				; 报错,"你不能放在这里"
    STR_WRONG DB 0AH,0DH,'Please Enter 1 OR 2!',0AH,0DH,'$'		 		; 错误信息的提示
    STR_COLOR DB 0AH,0DH,'Please Choose Your Chessman Color:(1 For Black, 2 For White)',0AH,0DH,'$'	; 选棋子的颜色, 1是黑色, 2是白色
	STR_PUT_WHITE DB '[White] ', '$'									; 用于提示当前是谁的回合
	STR_PUT_BLACK DB '[Black] ', '$'									; 用于提示当前是谁的回合
    STR_PUT DB 'Please Enter The Position(X Y):','$'					; 请输入棋子的位置(x,y)
    STR_GAMEEND DB 'One Player Has WIN!',0AH,0DH,'$'					; 游戏结束信息提示
	STR_WIN_B DB 'Black WINS!!', '$'
	STR_WIN_W DB 'White WINS!!', '$'
    STR_WIN DB 'You WIN! Congratulations!',0AH,0DH,'$'					; 游戏提示信息,"恭喜!你赢了!"
    STR_SORRY DB 'You LOSE! Donnot Give Up!',0AH,0DH,'$'				; 游戏提示信息,"对不起,你输了,不要放弃!"
    STR_WAIT1 DB 'Please Wait...',0AH,0DH,'$'  							; 进入等待信息提示 
    STR_CHOOSE1 DB 'WELCOME! Please Choose Game Mode:',0AH,0DH,'$'		; 游戏模式提示1
	STR_CHOOSE2 DB 'Press 1 For One Player (MAN VS CPU)',0AH,0DH,'Press 2 For Two Players (MAN VS MAN)',0AH,0DH,'Press ESC To Quit',0AH,0DH,'$'	; 游戏模式提示2
    STR_EXIT DB 0AH,0DH, 'One Player Has Quit!',0AH,0DH,'$'				; 一个玩家退出
	MUS_TYPE DB 1								                    	; 音乐类型; 0报错, 1胜利, 2失败
    MUS_FREQ0 DW 230,150,-1						                    	; 报错音频率表
	MUS_TIME0 DW 30,30							                    	; 报错音节拍表
	MUS_FREQ1 DW 270,270,270,190,230,270,250,270,-1						; 胜利音乐频率表, -1为音乐播放结束符
	MUS_TIME1 DW 3 DUP (30),50,50,30,30,80  							; 胜利音乐节拍
	MUS_FREQ2 DW 230, 260, 260, 260, 230, 200, 170, -1					; 失败音乐频率表, -1为音乐播放结束符
	MUS_TIME2 DW 50, 30, 30, 30, 30, 30, 50								; 失败音乐节拍

DATA ENDS
INISTACK SEGMENT STACK
	DW 128H DUP(0)								; 初始化堆栈						
INISTACK ENDS

ADDRESS MACRO A,B
     LEA SI,A
     LEA BP,DS:B
ENDM

GET_CB_ADDRESS MACRO A,B
	MOV AX,0
	MOV BX,0
	MOV AL,A
	MOV BL,15
	MUL BL
	ADD AL,B
	MOV BX,AX
ENDM

CODE SEGMENT
	ASSUME CS:CODE,DS:DATA,SS:INISTACK			; 说明一个对应的关系, 之后再把段的首地址赋值给段寄存器
START:
	MOV AX,DATA									; 数据库装入段寄存器DS
	MOV DS,AX
    MOV AL,2
	MOV AH,0									; 设置显示方式
	INT 10H

	MOV AH, 0BH
	MOV BH, 00H
	MOV BL, 00110000B
	INT 10H
	CALL PRINT									; 打印棋盘
SELECT:                                 		; 选择功能
    MOV DX,OFFSET STR_CHOOSE1                	; 选择双人或人机模式
	MOV AH,09H									; 使用21H中断的输出字符串功能
	INT 21H										; 在屏幕上显示输入的内容
	MOV DX,OFFSET STR_CHOOSE2                	; 选择双人或人机模式
	MOV AH,09H									; 使用21H中断的输出字符串功能
	INT 21H	
    MOV AH,1									; 使用21H中断的显示输入功能						
	INT 21H
	CMP AL,'2'                         			; 2为双人(人人)
	JE GAME1									; 输入为2, 双人模式
	CMP AL,'1'                          		; 1为单人(人机)
	JZ MARK										; 输入为1, 单人模式
	CMP AL,27                           		; 输入为ESC, 退出
    JZ GEND0									; 游戏结束
	MOV DX,OFFSET STR_WRONG      				; 输入不合规, 要求重新输入
	MOV AH,09H									; 在屏幕上显示内容
	INT 21H
	MOV MUS_TYPE, 0						    	; 修改音乐类型为警告
	CALL MUSIC									; 调用报错音
	JMP SELECT									; 回到选择功能
GEND0:
    MOV AH,4CH									; 退出游戏
	INT 21H
; =========/*双人*/========
GAME1:	                               
	MOV AL,2									; 在屏幕上显示输入的内容
	MOV AH,0
	INT 10H										; 设置80*25黑白方式, 清空屏幕
	CALL PRINT									; 打印棋盘
HERE1:
	MOV DX, OFFSET STR_PUT_BLACK
	CMP FLAG_NEXTSTEP, 0						; 当前为黑子回合
	JE PRINTTURN 
	MOV DX, OFFSET STR_PUT_WHITE				; 当前为白子回合
PRINTTURN:
	MOV AH,09H									; 在屏幕上显示当前是谁的回合
	INT 21H
	MOV DX,OFFSET STR_PUT						; 放置棋子的提示语句
	MOV AH,09H									; 在屏幕上显示输入的内容
	INT 21H
	MOV AH,1									; 若输入的是ESC则退出
	INT 21H
	CMP AL,27									; 若输入的是ESC
	JE QUIT										; 退出游戏
	JMP INPUTXY									; 否则输入坐标X Y
QUIT:											; 退出游戏的信息
	MOV STATE,2									; 把STATE的值设为2
	MOV DX,OFFSET STR_EXIT						; 有人退出就显示退出消息
	MOV AH,09H									; 使用21H号中断的显示输入功能
	INT 21H 
	JMP GEND1									; 游戏结束
MARK:											
    JMP GAME2									; 人机模式
MARK2:
	CMP FLAG_TRANS, 1							; 判断是否需要中转
	JE HERE1									; 需要中转
	MOV FLAG_TRANS, 0							; 无需中转, 重新设置FLAG
INPUTXY:										; 记录坐标X Y(ASCII码)
	MOV X,AL									; 记录x的坐标
	INT 21H										; 显示在屏幕上x的值
	CMP AL,27									; 若是ESC则退出
	JE QUIT										; 退出记录坐标x
	INT 21H										; 显示在屏幕上y的值
	CMP AL,27									; 若是ESC则退出
	JE QUIT										; 退出记录坐标y
	MOV Y,AL									; 记录y的坐标
N1:	MOV AH,07									; 无回显输入
	INT 21H
	CMP AL,27									; 若是ESC则退出
	JE QUIT										; 退出游戏
	CMP AL,13									; 若是回车则继续, 否则等待回车
	JNE N1										; 继续执行N1程序
	MOV AH,2
	MOV DL,0AH									; 显示光标的行坐标
	INT 21H										; 输出回车换行
	MOV DL,0DH									; 显示光标的列坐标
	INT 21H										; 输出回车换行
	CALL CHECK									; 检查可否落子, 将X, Y的ASCII值改变为真实的数值
	CMP FLAG_PUTDOWN,1							; 可以落子
	JE THERE1									; 可以落子则判断落子后输赢
	JMP HERE1									; 如果不可以落子则重新输入
THERE1:
	MOV MY,1									; 我的坐标是1, 对方的坐标是2
	CALL PUTDOWN								; 落子
	CALL ISWIN									; 判断输赢, 有结果则FLAG_OVER=1
	CALL PRINT									; 打印棋盘
	MOV FLAG_TRANS, 1							; 当前需要中转
	CMP FLAG_OVER,1								; 游戏结束																		
	JNZ MARK2					
	MOV FLAG_TRANS, 0							; 当前无需中转
	MOV DX,OFFSET STR_WIN_W						; 游戏结束的信息提示, 黑子获胜
	CMP FLAG_NEXTSTEP, 0
	JE PRINTRES
	MOV DX,OFFSET STR_WIN_B						; 游戏结束的信息提示, 白子获胜
PRINTRES:	
    MOV AH,09H									; 在屏幕上显示输入的内容
    INT 21H
    MOV AH,02H									; 使用10H中断的设置位置功能																							
    MOV DL,00H									; 设置光标的行坐标
    MOV DH,11H									; 设置光标的列坐标
    INT 10H
    MOV STATE,3									; 游戏结束我退出
	MOV MUS_TYPE,1								; 修改音乐类型为胜利
    CALL MUSIC									; 播放音乐
GEND1:
	MOV AH,4CH									; 退出游戏
	INT 21H
; =========/*人机*/========
GAME2:											; 人机模式
    MOV DX,OFFSET STR_COLOR               			
    MOV AH,09H									; 在屏幕上显示输入的内容
    INT 21H
    MOV AH,1									; 选择棋子颜色, 1黑子, 2白子
	INT 21H
	CMP AL,'1'
	JE CHESSBLACK								; 1为黑子
	CMP AL,'2'
	JE CHESSWHITE								; 2为白子
	CMP AL,27									; 若输入的是ESC
    JE GEND1									; 则退出
	MOV DX,OFFSET STR_WRONG						; 输入不合规, 要求重新输入
	MOV AH,09H									; 在屏幕上显示输入的内容
	INT 21H
	MOV MUS_TYPE, 0						    	; 修改音乐类型为警告
	CALL MUSIC									; 调用报错音
	JMP GAME2									; 游戏结束
CHESSBLACK:                               		; 若执黑则后行, 将我方棋子改为MY=1, ORDER改为2
    MOV MY,BLACK								; 我执黑棋, 对方执白棋
    MOV STATE,1									; 选的黑子，对方先下
    MOV ORDER,2									; 等待对方落子
CHESSWHITE:										; 若执白先行, 将我方棋子改为白MY=2, ORDER改为1
	MOV AL,2  
	MOV AH,0
	INT 10H										; 设置80*25黑白方式, 清空屏幕
	CALL PRINT									; 打印棋盘
HERE2:
	CMP STATE,0									; 根据STATE输出提示信息
	JE SHOW										; 应该是我下, 跳转至SHOW
	MOV DX,OFFSET STR_WAIT1						; 应该是对方下, 提示等待
	MOV AH,09H
	INT 21H										; 输出请等待的信息
	MOV AH,02H
	MOV DL,00H									; 光标从17,0开始
	MOV DH,11H									; 光标的列坐标
	INT 10H
WW:	
	CMP STATE,0									; 根据STATE输出提示信息
	JE SHOW										; 如果是我下, 转至SHOW
	CMP STATE,1	                                ; STATE为1, 则机器下
	JE ROBOT									; 跳转至ROBOT,机器落子
	CMP STATE,4									; 如果对方胜利
	JE ILOSE									; 																		
	CMP STATE,5									; 如果对方退出
	JE HQUIT									
	JMP WW
ROBOT:
    CALL FINDPLACE						        ; 机器寻找最优的落子位置
	CALL PUTDOWN							    ; 机器落子
	CALL ISWIN                                  ; 判断是否赢了
	CALL PRINT									; 打印棋盘
	CMP FLAG_OVER,1                             ; 判断游戏是否结束
	JE ILOSE									; 机器胜利
	MOV STATE,0									; 机器下完, 改变STATE
	JMP HERE2                                   ; 机器下完我下
SHOW:
	MOV DX, OFFSET STR_PUT_BLACK
	CMP FLAG_NEXTSTEP, 0						; 当前为黑子回合
	JE PRINTTURN1 
	MOV DX, OFFSET STR_PUT_WHITE				; 当前为白子回合
PRINTTURN1:
	MOV AH,09H									; 在屏幕上显示当前是谁的回合
	INT 21H
	MOV DX,OFFSET STR_PUT						; 放置棋子的提示语句
	MOV AH,09H									; 在屏幕上显示输入的内容
	INT 21H
	MOV AH,1									; 若输入的是ESC则退出
	INT 21H
	CMP AL,27									; 若输入的是ESC
	JE IQUIT									; 我退出
	JMP INPUTXY2								; 否则输入坐标X Y
ILOSE:											; “我输了”的处理程序
	MOV DX,OFFSET STR_SORRY						; 提示抱歉信息
	MOV AH,09H									; 在屏幕上显示输入的内容
	INT 21H
	MOV MUS_TYPE,2								; 修改音乐类型为失败
	CALL MUSIC									; 调用播放音乐
	JMP GEND2									; 游戏结束信息提示
IQUIT:											; “我退出”的处理程序
	MOV STATE,3									; 对方已获胜
HQUIT:											; “对方退出”的处理程序
	MOV DX,OFFSET STR_EXIT						; 有人退出就显示退出消息
	MOV AH,09H									; 在屏幕上显示对方退出的信息
	INT 21H
	JMP GEND2									; 游戏结束信息提示
INPUTXY2:										; 记录坐标X Y(ASCII码)
	MOV X,AL									; 显示x的坐标在屏幕上					
	INT 21H
	CMP AL,27									; 若是ESC则退出
	JE IQUIT									; 我退出
	INT 21H
	CMP AL,27									; 若是ESC则退出
	JE IQUIT									; 我退出
	MOV Y,AL									; 显示y的坐标在屏幕上
N2:	MOV AH,07									; 无回显输入
	INT 21H
	CMP AL,27									; 如果是ESC
	JE IQUIT									; 退出
	CMP AL,13									; 如果是回车, 继续；
	JNE N2										; 如果不是回车, 则循环等待
	MOV AH,2
	MOV DL,0AH									; 输出回车换行
	INT 21H
	MOV DL,0DH									; 光标的行坐标
	INT 21H										; 输出回车换行							
	CALL CHECK									; 检查可否落子
	CMP FLAG_PUTDOWN,1							; FLAG_PUTDOWN=1, 可以落子
	JE THERE2									; 可以落子则判断落子
	JMP HERE2									; 如果不可以落子则重新输入
THERE2:
    CMP ORDER,2									
    JE L1										; 如果选择后手落子
 	MOV MY,WHITE								; 否则我是白棋, 对方是黑棋
 	JMP L2
L1: 
    MOV MY,BLACK								; 我是黑棋, 对方是白棋
L2:
	CALL PUTDOWN								; 落子
	CALL ISWIN									; 判断输赢, 赢了则FLAG_OVER=1
	CALL PRINT									; 打印棋盘                                             
	CMP FLAG_OVER,1								; 判断我是否赢了
	JE IWIN										; 赢了, 则跳转到IWIN子程序
	MOV STATE,1									; 否则将STATE置1, 表示我已下完
	JMP HERE2
IWIN:											; 我赢了则显示祝贺信息, 播放胜利音乐
	MOV DX,OFFSET STR_WIN						; 获胜信息显示
	MOV AH,09H									; 在屏幕上显示输入的内容
	INT 21H
	MOV AH,02H
	MOV DL,00H									; 光标从16,0开始
	MOV DH,10H									; 
	INT 10H										; 修改光标位置
	MOV STATE,2									; 修改状态为胜利	
	MOV MUS_TYPE, 1								; 修改音乐类型为胜利				
	CALL MUSIC									; 播放音乐
GEND2:
	MOV AH,4CH									; 退出游戏
	INT 21H
; =========/*检验落子位置是否合法*/========
CHECK PROC NEAR									; 检验落子位置是否合法
	PUSH AX										; 保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV FLAG_PUTDOWN,1							; 初始化FLAG_PUTDOWN为1
    MOV CX,2                                 	; 循环两次,分别判断X和Y是否合法
	MOV AL,X                                 	; X 放入 AL
CMPD:	                                     	; 大小写转换, 检查是否在棋盘范围内
	DEC CX                                   	; CX 减一
	CMP AL,'A'                               	; 判断 AL 是否小于 'A'
	JL CMPLA                                 	; 如果小于, 跳转到 CMPLA
	CMP AL,'F'								 	; 判断 AL 是否小于 'F'
	JL CMPLF                                 	; 如果小于, 跳转到 CMPLF
	CMP AL,'a'                               	; 判断 AL 是否小于 'a'
	JL INPUTERR                              	; 如果小于, 报错
	CMP AL,'f'                               	; 判断 AL 是否大于 'f'
	JG INPUTERR                              	; 如果大于, 报错
	SUB AL,32                                	; 小写转换为大写
	SUB AL,7                                 	; 减去 '9' 与 'A' 的差值
CMPCX:	                                     	; 比较 CX,判断是否继续比较
	CMP CX,0                                 	; 判断 CX 是否为 0
	JE SUBXY                                 	; 如果等于零, Y已经检查过, 跳转到 SUBXY
	MOV X,AL                                 	; 将改变后的 AL 放入 X
	MOV AL,Y                                 	; Y 放入 AL
	JMP CMPD                                 	; 检查 Y
CMPLA:                                       	; AL 小于 'A',检查是否在0-9范围内
	CMP AL,'1'                               	; 判断 AL 是否小于 '1'
	JL INPUTERR                              	; 如果小于, 报错
	CMP AL,'9'                               	; 判断 AL 是否大于 '9'
	JG INPUTERR                              	; 如果大于, 报错
	JMP CMPCX                                	; 判断 Y 是否检查过
CMPLF:                                       	; AL 在 A-F 之间
	SUB AL,7                                 	; 减去 '9' 与 'A' 的差值
	JMP CMPCX                                	; 判断 Y 是否检查过
SUBXY: 	
	MOV Y,AL                                 	; 将改变后的 AL 放入 Y
    SUB X,'1'                            	 	; 将X改变为真实的值
	SUB Y,'1'								 	; 将Y改变为真实的值
	GET_CB_ADDRESS X,Y                          ; 获取点X,Y的地址,放到BX
	CMP CHESSBOARD[BX],BLACK                 	; 若此处已有棋子, 输入不合法
	JE INPUTERR							
	CMP CHESSBOARD[BX],WHITE					
	JNE RETURNC  								; 若此处没有棋子, 输入合法
INPUTERR:
    MOV FLAG_PUTDOWN,0                          ; 对于不合法的输入, 显示错误信息, 并播放报错音乐
	MOV DX,OFFSET STR_ERROR
	MOV AH,09H									; 在屏幕上显示输入错误的信息
    INT 21H
	MOV MUS_TYPE, 0						    	; 修改音乐类型为警告
	CALL MUSIC									; 调用报错音
RETURNC:
    POP DX										; 恢复CPU现场
    POP CX
    POP BX
    POP AX
	RET										    ; 子程序结束返回
CHECK ENDP
; =========/*落子子程序*/========
PUTDOWN PROC NEAR													
	PUSH AX										; 保存CPU现场
	PUSH BX
    GET_CB_ADDRESS X,Y                          ; 获取点X,Y的地址,放到BX
	CMP FLAG_NEXTSTEP,0                         ; 根据FLAG_NEXTSTEP值, 轮流放置黑子和白子
	JE PUTBLACK
	MOV CHESSBOARD[BX],WHITE					; 放白棋
	MOV FLAG_NEXTSTEP,0							; 改变FLAG_NEXTSTEP的值, 下一步为黑子
	JMP ENDPUT
PUTBLACK:	
    MOV CHESSBOARD[BX],BLACK					; 放黑棋
    MOV FLAG_NEXTSTEP,1							; 改变FLAG_NEXTSTEP的值, 下一步为白子
ENDPUT:
    INC FLAG_NUM	                            ; 棋子数量加1
	POP BX
	POP AX
	RET										    ; 子程序结束返回
PUTDOWN ENDP
; =====/*机器寻找落子的位置*/=======
FINDPLACE PROC NEAR
	PUSH AX										; 保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV X,0                                     ; 初始化 X,Y
	MOV Y,0
	MOV X1,0 								    ; 初始化 X1,Y1	
	MOV Y1,0
	MOV SCORE,1                                 ; 初始化分数
	MOV AX,20000
	NEG AX                                      ; 取负数
	MOV MAXSCORE1,AX							; 初始化最大分数
	CMP MY,1        							; 判断机器黑棋还是白棋
    JE IMBLACK
	MOV DL,BLACK    							; 机器是黑棋
	MOV DH,WHITE    							; 玩家是白棋
    JMP ISFIRST
IMBLACK:
    MOV DL,WHITE    							; 机器是白棋
	MOV DH,BLACK    							; 玩家是黑棋
ISFIRST:
	CMP FLAG_NUM,0                              ; 判断是不是首颗棋子
	JNE FIND                                   	; 不是首颗棋子, 跳转到FIND
	MOV BL,7                                   	; 是首颗棋子, 则下正中间的位置
	MOV BH,7
	JMP HASFIND                                	; 已经找到落子位置, 停止寻找, 跳转到HASFIND
FIND:
	MOV AL,X                                   
	MOV X1,AL                                  	; 临时保存现在的坐标x到x1
	MOV AL,Y
	MOV Y1,AL                                  	; 临时保存现在的坐标y到y1
	CALL CHECKPLACE 						    ; 检查当前坐标是否合法
	CMP FLAG_PUTDOWN,0                          ; 如果不能落子
    JE NEXTY                                    ; 则继续寻找
	MOV SCORE,1                                 ; 重置分数
	CALL CALSCORE							    ; 计算当前位置分数
	CMP SCORE,10000                             ; 若有五连
	JGE ENDFIND                                 ; 结束寻找
	MOV AX,SCORE
	XCHG DL,DH                                 	; 第二层网络交换要计算的颜色
	CALL FINDPLACE2								; 计算第二层网络分数最大分数保存到MAXSCORE2
	XCHG DL,DH                                  ; 第二层网络计算完分数后换回来
	;  CMP MAXSCORE2,10000                      ; 如果有五连
	;  JGE ENDFIND                              ; 结束寻找
	SUB AX,MAXSCORE2                            ; 第一层的分数减去第二层的最大分数, 即为当前位置的分数
	CMP AX,MAXSCORE1						    
    JLE NEXTY									; 如果当前位置分数小于最高分数,则继续寻找
	MOV MAXSCORE1,AX							; 如果当前位置分数大于最高分数,则替换最高分数
	MOV BL,X1                                	; 保存当前最高分数的位置x1
	MOV BH,Y1                                   ; 保存当前最高分数的位置y1
NEXTY:
    MOV AL,X1								    ; 还原当前的位置x1,y1到x,y
	MOV X,AL
	MOV AL,Y1
	MOV Y,AL
    INC Y                                   	; 右移一位
	CMP Y,15									; 判断是否到达最后一列
    JNE FIND									; 不是则继续寻找
	MOV Y,0                                		; 从头开始
	INC X                                  		; 下一行
	CMP X,15                              		; 判断是否已经到达最后一行
	JNE FIND                               		; 如果没有到达最后一行,则继续
    JMP HASFIND
ENDFIND:
	MOV BL,X
	MOV BH,Y
HASFIND:
	MOV X,BL                               		; 保存最高分数的位置x
	MOV Y,BH                               		; 保存最高分数的位置y
    POP DX										; 恢复CPU现场
	POP CX
	POP BX
	POP AX
	RET
FINDPLACE ENDP
; =====/*第二层网络*/=======
FINDPLACE2 PROC NEAR
	PUSH AX										; 保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	GET_CB_ADDRESS X1,Y1   			  			; 获取点X1,Y1的地址,放到BX
	MOV AX,0
	MOV AL,CHESSBOARD[BX]
	MOV CB1,AL                         			; 保存第一层网络落子的位置棋子的内容
	MOV CHESSBOARD[BX],DH             			; 在第一层网络选择的位置落子
	MOV X,0                                     ; 初始化x,y
	MOV Y,0
	MOV AX,0
	MOV SCORE,1                                 ; 初始化分数
	MOV MAXSCORE2,0							    ; 初始化最大分数
FIND2:
	CALL CHECKPLACE 						    ; 检查当前坐标是否合法
	CMP FLAG_PUTDOWN,0                          ; 如果不能落子
    JE NEXTY2                                   ; 则继续寻找
	MOV SCORE,1                                 ; 重置分数
	CALL CALSCORE							    ; 计算当前位置分数
	MOV CX,SCORE
	CMP CX,10000                                ; 若有五连
	JGE ENDFIND2                                ; 则结束寻找, 否则继续
	CMP CX,MAXSCORE2						    
    JLE NEXTY2									; 如果当前位置分数小于最高分数,则继续
	MOV MAXSCORE2,CX							; 如果当前位置分数大于最高分数,则替换最高分数
	MOV AL,X                                    ; 保存当前最高分数的位置x
	MOV AH,Y                                    ; 保存当前最高分数的位置y
NEXTY2:
    INC Y                                   	; 右移一位
	CMP Y,15								    ; 判断是否到达最后一列
    JNE FIND2                                   ; 不是则继续寻找
	MOV Y,0                                		; 从头开始
	INC X                                  		; 下一行
	CMP X,15                               		; 判断是否已经到达最后一行
	JNE FIND2                               	; 如果没有到达最后一行,则继续
	JMP HASFIND2
ENDFIND2:
	MOV AL,X
	MOV AH,Y
	MOV MAXSCORE2,10000                         ; 设置最高分数为10000
HASFIND2:
	MOV X,AL                               		; 保存最高分数的位置x
	MOV Y,AH                               		; 保存最高分数的位置y
	MOV AL,CB1
	MOV CHESSBOARD[BX],AL                  		; 还原
    POP DX										; 恢复CPU现场
	POP CX
	POP BX
	POP AX
	RET
FINDPLACE2 ENDP
; =====/*检查机器落子位置是否合法*/=======
CHECKPLACE PROC NEAR
	PUSH AX										; 保存CPU现场
	PUSH BX
	MOV FLAG_PUTDOWN,1							; 初始化FLAG_PUTDO为1
	CMP X,0
	JL INPUTERR2							    ; 如果x<0,则输入错误
	CMP X,14
	JG INPUTERR2                                ; 如果x>14,则输入错误
	CMP Y,0
	JL INPUTERR2                                ; 如果y<0,则输入错误
	CMP Y,14
	JG INPUTERR2                                ; 如果y>14,则输入错误
	GET_CB_ADDRESS X,Y                          ; 获取点X,Y的地址,放到BX
	CMP CHESSBOARD[BX],BLACK                 	
	JE INPUTERR2							    ; 若此处已有棋子, 输入不合法
	CMP CHESSBOARD[BX],WHITE					
	JNE FINISHCHECK								; 若此处没有棋子, 输入合法
INPUTERR2:
    MOV FLAG_PUTDOWN,0                          ; 对于不合法的输入, 显示错误信息
FINISHCHECK:					                                  
	POP BX
	POP AX
	RET
CHECKPLACE ENDP
; =====/*计算当前位置分数*/=======
; 输入：DL存当前位置要算的棋子的颜色, DH存另一棋子的颜色
CALSCORE PROC NEAR
	PUSH AX										; 保存CPU现场
	PUSH BX
	PUSH CX
	PUSH DX
	MOV SCORE,1
	GET_CB_ADDRESS X,Y                          ; 获取点X,Y的地址,放到BX
	MOV CX,8       								; 计算分数, 循环八次
	CALL SCORE1
	CMP CHESSMODEL[16],1   						; 如果有一个能五连
	JGE FINISHCAL          						; 则结束计算
	CALL SCORE2
	CMP CHESSMODEL[16],1 
	JGE FINISHCAL
	CALL SCORE3
	CMP CHESSMODEL[16],1
	JGE FINISHCAL
	CALL SCORE4
	CMP CHESSMODEL[16],1
	JGE FINISHCAL
	CALL SCORE5
	CMP CHESSMODEL[16],1
	JGE FINISHCAL
	CALL SCORE6
	CMP CHESSMODEL[16],1
	JGE FINISHCAL
	CALL SCORE7
	CMP CHESSMODEL[16],1
	JGE FINISHCAL
	CALL SCORE8
FINISHCAL:
	CLC
    MOV AX,0                					; 清零
	MOV DX,0                					; 清零
	MOV BX,CX               					; 第cx个
	MOV AL,CHESSMODEL[BX]   					; 第bx个棋形的个数
	ADD BX,BX               					; CHESSSCORE是word类型, 所以偏移地址为2BX
    MUL CHESSSCORE[BX]      					; chessscore[bx]*chessmodel[bx]
	ADD SCORE,AX            					; 加到总分
	LOOP FINISHCAL          					; 循环计算
	MOV AX,0
	MOV AL,CHESSMODEL[0]   						; 计算第一个棋形的分数
	MUL CHESSSCORE[0]
	ADD SCORE,AX
	MOV CX,8               						; 循环8次清0
CLEARCM:                   						; 清空棋形数组
	MOV BX,CX
	MOV CHESSMODEL[BX],0
	LOOP CLEARCM
	MOV CHESSMODEL[0],0	
    POP DX
	POP CX
	POP BX
	POP AX
	RET
CALSCORE ENDP
; =========/*判断东方向的棋形*/========
SCORE1 PROC NEAR								; 横向判断子程序
    PUSH BX										; 保存cpu现场
    CMP Y,10	 								; 判断横向是否有10个字节
    JG  SC10									; 若小于则横向不能连成5个
    CMP DL,CHESSBOARD[BX+1]						; 判断棋盘横向是否有2个棋子连在一起
    JNE SC11
    CMP DL,CHESSBOARD[BX+2]						; 判断棋盘横向是否有3个棋子连在一起
    JNE SC12 
    CMP DL,CHESSBOARD[BX+3]						; 判断棋盘横向是否有4个棋子连在一起
    JNE SC13
    CMP DL,CHESSBOARD[BX+4]						; 判断棋盘横向是否有5个棋子连在一起
    JNE SC14
    INC CHESSMODEL[8]							; 1111,10000分
	JMP SC10
SC11:
    CMP DH,CHESSBOARD[BX+1]
	JE SC111
	INC CHESSMODEL[1]                          	; 0XXX  
	JMP SC10
SC111:
	INC CHESSMODEL[0]                          	; 2XXX  
	JMP SC10
SC12:
    CMP DH,CHESSBOARD[BX+2]
	JE SC121
	INC CHESSMODEL[3]                           ; 10XX
	JMP SC10
SC121:
	INC CHESSMODEL[2]                           ; 12XX
	JMP SC10
SC13:
	CMP DH,CHESSBOARD[BX+3]
	JE SC131
	INC CHESSMODEL[5]                           ; 110X
	JMP SC10
SC131:
    INC CHESSMODEL[4]                           ; 112X
	JMP SC10	
SC14:
	CMP DH,CHESSBOARD[BX+4]
	JE SC141
	INC CHESSMODEL[7]                           ; 1110
	JMP SC10
SC141:
	INC CHESSMODEL[6]                           ; 1112
SC10: 
    POP BX										; 恢复cpu现场
    RET											; 子程序结束返回 
SCORE1 ENDP
; =========/*判断南方向的棋形*/========
SCORE2 PROC NEAR								; 纵向判断子程序
   PUSH BX										; 保存cpu现场
   CMP X,10										; 判断纵向是否有10个字节
   JG SC20										; 若小于则纵向不能连成5个
   CMP DL,CHESSBOARD[BX+15]						; 判断棋盘纵向是否有2个棋子连在一起
   JNE SC21
   CMP DL,CHESSBOARD[BX+30]						; 判断棋盘纵向是否有3个棋子连在一起
   JNE SC22
   CMP DL,CHESSBOARD[BX+45]						; 判断棋盘纵向是否有4个棋子连在一起
   JNE SC23
   CMP DL,CHESSBOARD[BX+60]						; 判断棋盘纵向是否有5个棋子连在一起
   JNE SC24
   INC CHESSMODEL[8]   							; 1111,10000分
SC21:
   CMP DH,CHESSBOARD[BX+15]
   JE SC211
   INC CHESSMODEL[1]                            ; 0XXX  
   JMP SC20	
SC211:
	INC CHESSMODEL[0]                           ; 2XXX  
    JMP SC20
SC22:
   	CMP DH,CHESSBOARD[BX+30]
   	JE SC221
   	INC CHESSMODEL[3]                           ; 10XX
	JMP SC20
SC221:
	INC CHESSMODEL[2]                           ; 12XX
    JMP SC20
SC23:
	CMP DH,CHESSBOARD[BX+45]
	JE SC231
	INC CHESSMODEL[5]                           ; 110X
	JMP SC20
SC231:
	INC CHESSMODEL[4]                           ; 112X
	JMP SC20
SC24:
	CMP DH,CHESSBOARD[BX+60]
	JE SC241
	INC CHESSMODEL[7]                           ; 1110
	JMP SC20
SC241:
	INC CHESSMODEL[6]                           ; 1112
SC20: 
   POP BX
   RET											; 子程序结束返回
SCORE2 ENDP
; =========/*判断东北方向的棋形*/========
SCORE3 PROC NEAR								; 斜上判断子程序
   PUSH BX										; 保存cpu现场
   CMP X,4		      							; 判断纵向是否有4个字节                  																	
   JL SC30										; 若小于则斜上不能连成5个
   CMP Y,10										; 判断横向是否有10个字节
   JG SC30
   CMP DL,CHESSBOARD[BX-14]						; 判断棋盘斜上是否有2个棋子连在一起
   JNE SC31
   CMP DL,CHESSBOARD[BX-28]						; 判断棋盘斜上是否有3个棋子连在一起
   JNE SC32
   CMP DL,CHESSBOARD[BX-42]						; 判断棋盘斜上是否有4个棋子连在一起
   JNE SC33
   CMP DL,CHESSBOARD[BX-56]						; 判断棋盘斜上是否有5个棋子连在一起
   JNE SC34
   INC CHESSMODEL[8]  							; 1111,10000分
SC31:
	CMP DH,CHESSBOARD[BX-14]
   	JE SC311
   	INC CHESSMODEL[1]                           ; 0XXX  
   	JMP SC30
SC311:
	INC CHESSMODEL[0]                           ; 2XXX  
	JMP SC30
SC32:
   	CMP DH,CHESSBOARD[BX-28]
   	JE SC321
   	INC CHESSMODEL[3]                           ; 10XX
	JMP SC30
SC321:
	INC CHESSMODEL[2]                           ; 12XX
	JMP SC30
SC33:
	CMP DH,CHESSBOARD[BX-42]
	JE SC331
	INC CHESSMODEL[5]                           ; 110X
	JMP SC30
SC331:
	INC CHESSMODEL[4]                           ; 112X
	JMP SC30
SC34:
	CMP DH,CHESSBOARD[BX-56]
	JE SC341
	INC CHESSMODEL[7]                           ; 1110
	JMP SC30
SC341:
	INC CHESSMODEL[6]                           ; 1112
SC30: 
   POP BX
   RET											; 子程序结束返回
SCORE3 ENDP
; =========/*判断东南方向的棋形*/========
SCORE4 PROC NEAR								; 斜下判断子程序
   	PUSH BX										; 保存cpu现场
   	CMP X,10								    ; 判断纵向是否有10个字节
   	JG SC40										; 若小于则斜下不能连成5个
   	CMP Y,10									; 判断横向是否有10个字节
   	JG SC40         							; 若小于则斜下不能连成5个
   	CMP DL,CHESSBOARD[BX+16]					; 判断棋盘斜下是否有2个棋子连在一起 
   	JNE SC41
   	CMP DL,CHESSBOARD[BX+32]					; 判断棋盘斜下是否有3个棋子连在一起
   	JNE SC42
   	CMP DL,CHESSBOARD[BX+48]					; 判断棋盘斜下是否有4个棋子连在一起
   	JNE SC43
   	CMP DL,CHESSBOARD[BX+64]					; 判断棋盘斜下是否有5个棋子连在一起
	JNE SC44
	INC CHESSMODEL[8]   						; 1111,10000分
SC41:
	CMP DH,CHESSBOARD[BX+16]
   	JE SC411
   	INC CHESSMODEL[1]                           ; 0XXX  
   	JMP SC40
SC411:
	INC CHESSMODEL[0]                           ; 2XXX  
	JMP SC40
SC42:
   	CMP DH,CHESSBOARD[BX+32]
   	JE SC421
   	INC CHESSMODEL[3]                           ; 10XX
SC421:
	INC CHESSMODEL[2]                           ; 12XX
	JMP SC40
SC43:
	CMP DH,CHESSBOARD[BX+48]
	JE SC431
	INC CHESSMODEL[5]                           ; 110X
	JMP SC40
SC431:
	INC CHESSMODEL[4]                           ; 112X
	JMP SC40
SC44:
    CMP DH,CHESSBOARD[BX+64]
	JE SC441
	INC CHESSMODEL[7]                           ; 1110
SC441:
	INC CHESSMODEL[6]                           ; 1112																		
SC40: 
   POP BX
   RET											; 子程序结束返回
SCORE4 ENDP
; =========/*判断西方向的棋形*/========
SCORE5 PROC NEAR
	PUSH BX
	CMP Y,4
	JL SC50
	CMP DL,CHESSBOARD[BX-1]
	JNE SC51
	CMP DL,CHESSBOARD[BX-2]
	JNE SC52
	CMP DL,CHESSBOARD[BX-3]
	JNE SC53
	CMP DL,CHESSBOARD[BX-4]
	JNE SC54
	INC CHESSMODEL[8]  						    ; 1111,10000分
	JMP SC50
SC51:
	CMP DH,CHESSBOARD[BX-1]
	JE SC511
	INC CHESSMODEL[1]                           ; 0XXX  
	JMP SC50
SC511:
	INC CHESSMODEL[0]                           ; 2XXX  
	JMP SC50
SC52:
   	CMP DH,CHESSBOARD[BX-2]
   	JE SC521
   	INC CHESSMODEL[3]                           ; 10XX
	JMP SC50
SC521:
	INC CHESSMODEL[2]                           ; 12XX
	JMP SC50
SC53:
	CMP DH,CHESSBOARD[BX-3]
	JE SC531
	INC CHESSMODEL[5]                           ; 110X
	JMP SC50
SC531:
	INC CHESSMODEL[4]                           ; 112X
	JMP SC50
SC54:
	CMP DH,CHESSBOARD[BX-4]
	JE SC541
	INC CHESSMODEL[7]                           ; 1110
	JMP SC50
SC541:
	INC CHESSMODEL[6]                           ; 1112
SC50: 
   POP BX
   RET											; 子程序结束返回
SCORE5 ENDP
; =========/*判断北方向的棋形*/========
SCORE6 PROC NEAR
	PUSH BX
	CMP X,4
	JL SC60
	CMP DL,CHESSBOARD[BX-15]
	JNE SC61
	CMP DL,CHESSBOARD[BX-30]
	JNE SC62
	CMP DL,CHESSBOARD[BX-45]
	JNE SC63
	CMP DL,CHESSBOARD[BX-60]
	JNE SC64
	INC CHESSMODEL[8]  							; 1111,10000分
	JMP SC60
SC61:
	CMP DH,CHESSBOARD[BX-15]
	JE SC611
	INC CHESSMODEL[1]                          	; 0XXX  
	JMP SC60
SC611:
	INC CHESSMODEL[0]                        	; 2XXX  
	JMP SC60
SC62:
   	CMP DH,CHESSBOARD[BX-30]
   	JE SC621
   	INC CHESSMODEL[3]                         	; 10XX
	JMP SC60
SC621:
	INC CHESSMODEL[2]                          	; 12XX
	JMP SC60
SC63:
	CMP DH,CHESSBOARD[BX-45]
	JE SC631
	INC CHESSMODEL[5]                      		; 110X
	JMP SC60
SC631:	
	INC CHESSMODEL[4]                          	; 112X
	JMP SC60
SC64:	
	CMP DH,CHESSBOARD[BX-60]
	JE SC641
	INC CHESSMODEL[7]                        	; 1110
	JMP SC60
SC641:	
	INC CHESSMODEL[6]                          	; 1112
SC60: 
   POP BX
   RET											; 子程序结束返回
SCORE6 ENDP
; =========/*判断西北方向的棋形*/========
SCORE7 PROC NEAR
	PUSH BX
	CMP X,4
	JL SC70
	CMP Y,4
	JL SC70
	CMP DL,CHESSBOARD[BX-16]
	JNE SC71
	CMP DL,CHESSBOARD[BX-32]
	JNE SC72
	CMP DL,CHESSBOARD[BX-48]
	JNE SC73
	CMP DL,CHESSBOARD[BX-64]
	JNE SC74
	INC CHESSMODEL[8]  							; 1111,10000分
	JMP SC70
SC71:
	CMP DH,CHESSBOARD[BX-16]
	JE SC711
	INC CHESSMODEL[1]                          	; 0XXX  
	JMP SC70
SC711:
	INC CHESSMODEL[0]                        	; 2XXX  
	JMP SC70
SC72:
   	CMP DH,CHESSBOARD[BX-32]
   	JE SC721
   	INC CHESSMODEL[3]                         	; 10XX
	JMP SC70
SC721:
	INC CHESSMODEL[2]                          	; 12XX
	JMP SC70
SC73:	
	CMP DH,CHESSBOARD[BX-48]
	JE SC731
	INC CHESSMODEL[5]                      		; 110X
	JMP SC70
SC731:	
	INC CHESSMODEL[4]                          	; 112X
	JMP SC70
SC74:
	CMP DH,CHESSBOARD[BX-64]
	JE SC741
	INC CHESSMODEL[7]                        	; 1110
	JMP SC70
SC741:
	INC CHESSMODEL[6]                          	; 1112
SC70: 
   POP BX
   RET											; 子程序结束返回
SCORE7 ENDP
; =========/*判断西南方向的棋形*/========
SCORE8 PROC NEAR
	PUSH BX
	CMP X,10
	JG SC80
	CMP Y,4
	JL SC80
	CMP DL,CHESSBOARD[BX+14]
	JNE SC81
	CMP DL,CHESSBOARD[BX+28]
	JNE SC82
	CMP DL,CHESSBOARD[BX+42]
	JNE SC83
	CMP DL,CHESSBOARD[BX+56]
	JNE SC84
	INC CHESSMODEL[8]  							; 1111,10000分
	JMP SC80
SC81:
	CMP DH,CHESSBOARD[BX+14]
	JE SC811
	INC CHESSMODEL[1]                          	; 0XXX  
	JMP SC80
SC811:
	INC CHESSMODEL[0]                        	; 2XXX  
	JMP SC80
SC82:
   	CMP DH,CHESSBOARD[BX+28]
   	JE SC821
   	INC CHESSMODEL[3]                         	; 10XX
	JMP SC80
SC821:	
	INC CHESSMODEL[2]                          	; 12XX
	JMP SC80
SC83:	
	CMP DH,CHESSBOARD[BX+42]
	JE SC831
	INC CHESSMODEL[5]                      		; 110X
	JMP SC80
SC831:
	INC CHESSMODEL[4]                          	; 112X
	JMP SC80
SC84:
	CMP DH,CHESSBOARD[BX+56]
	JE SC841
	INC CHESSMODEL[7]                        	; 1110
	JMP SC80
SC841:
	INC CHESSMODEL[6]                          	; 1112
SC80: 
   POP BX
   RET											; 子程序结束返回
SCORE8 ENDP
; =========/*判断是否获胜*/========
ISWIN PROC NEAR
	PUSH AX
	PUSH BX
	PUSH CX
	PUSH DX										
    MOV X,0										; 初始化X和Y
    MOV Y,0
	MOV FLAG_OVER,0   							; 初始化FLAG_OVER
LOOPY:
	GET_CB_ADDRESS X,Y                          ; 获取点X,Y的地址,放到BX
	MOV DL,CHESSBOARD[BX]               																	
	CMP FLAG_NEXTSTEP,0                         ; FLAG_NEXTSTEP=0,判断白子是否获胜
	JZ WISWIN
	CMP DL,BLACK								; 判断黑子是否获胜
	JE PANDUAN									; 判断黑子是否可以连成5个
	JMP NEXT									; 进入下一轮判断
WISWIN:
	CMP DL,WHITE								; 判断白子是否获胜
    JE PANDUAN									; 判断白子是否连成5个
	JMP NEXT 									; 进入下一轮判断
PANDUAN: 										; 游戏胜利的判断
    CALL SCORE1                          		; 横着
	CMP CHESSMODEL[8],1							; 横着连成5个游戏结束
	JGE RETURNISWIN								; 返回胜利的判断
	CALL SCORE2                         		; 竖着
    CMP CHESSMODEL[8],1							; 竖着连成5个游戏结束
	JGE RETURNISWIN								; 返回胜利的判断
	CALL SCORE3                          		; 斜上
	CMP CHESSMODEL[8],1							; 斜上连成5个游戏结束
	JGE RETURNISWIN								; 返回胜利的判断
	CALL SCORE4                          		; 斜下
    CMP CHESSMODEL[8],1							; 斜下连成5个游戏结束
	JGE RETURNISWIN								; 返回胜利的判断
NEXT:
    MOV CHESSMODEL[8],0 
    INC Y										; 右移一位
	CMP Y,15									; 是否到达最后一列
	JNE LOOPY                                   ; 不是则继续
	MOV Y,0										; 从头开始
	INC X										; 下一行
	CMP X,15									; 是否到达最后一行
	JNE LOOPY                                   ; 不是则继续
	MOV CX,8
	JMP ENDISWIN
RETURNISWIN:
	MOV FLAG_OVER,1   							; 游戏结束
	MOV CX,8                                    
ENDISWIN:
    MOV BX,CX                             
	MOV CHESSMODEL[BX],0                        ; CHESSMODEL清零
	LOOP ENDISWIN
	MOV CHESSMODEL[0],0
    POP DX
	POP CX
	POP BX    
	POP AX
    RET											; 子程序结束返回
ISWIN ENDP	 
; =========/*打印棋盘*/========
PRINT PROC NEAR									; 打印棋盘
	PUSH SI
	PUSH AX										; 保存CPU现场
	PUSH DX
	MOV AH,02H									; 使用INT 10H(BIOS中断)的设置光标位置功能:DH=行, DL=列
	MOV DL,00H									; 光标从0,0开始
    MOV DH,00H										
    INT 10H	
	MOV SI, 0
PRFIRSTL:
    MOV DX,OFFSET FIRSTLINE						; 打印棋盘的Y坐标(即最上面一行)  
	MOV DL, FIRSTLINE[SI]
	CMP DL, '$'
	JE PRNEXT
	MOV BL, 01100011B							; 修改字体颜色为青色, 背景为橙色
    CALL MYOUTPUT								; 调用自定义输出程序
	INC SI
	JMP PRFIRSTL
PRNEXT:
	MOV AH, 03H									; 10H中断;  功能号03获取光标坐标
	INT 10H										
	INC DH										
	XOR DL, DL
	MOV AH, 02H									; 10H中断;  功能号02设置光标坐标
	INT 10H
	MOV X,0										; 初始化X Y SI
	MOV Y,0
	MOV SI,0
LOOP2: 											; 打印第一列(y=0),即每行的首位
    CMP Y,0										; 判断Y是否为0
    JNE NOTHEAD									; 如果是0, 进入打印第一列的程序;  否则跳过
    MOV DL,X									
    ADD DL,31H									; 将X从坐标(纯数字)转化成对应的ASCII字符
	CMP DL,'9'									; 如果字符是'1'~'9', 直接跳转至PP打印
	JLE PP
	ADD DL,7									; 如果字符是'A'~..., 加上7转化后再打印 
PP:
	MOV BL, 01100011B
    CALL MYOUTPUT								
NOTHEAD:										; 打印棋盘中间部分, 循环打印每一行
    MOV DL,CHESSBOARD[SI]						; Y和SI表示记录所在的列(y坐标)
	MOV BL, 01100111B							; 属性: IRGB|IRGB, 前为背景、后为字体;  I表示高亮	
	CMP DL, 1									; 如果是黑棋子
	JNE CALLPRINT
	MOV BL, 01100000B							; 更改颜色为黑色(白子无需改, 默认为白色)
	MOV DL, 2
CALLPRINT:
	CALL MYOUTPUT								; 自定义的打印程序, 可打印彩色字符并自动修改光标位置										
	INC SI										; 打印下一个字符, 即Y+=1,SI+=1
	INC Y										
	CMP Y,15									; 如果到达当前行的末尾, 则跳转至换行程序
	JE NEXTLINE
	MOV DL, '-'									; 打印分隔棋子用的'-'
	MOV BL, 01100111B							; 属性: IRGB|IRGB, 前为背景、后为字体;  I表示高亮
	CALL MYOUTPUT
	JMP LOOP2									; 回到循环2
NEXTLINE:
    MOV DL,32
    MOV AH,02H
	INT 21H
	MOV DL,0AH									; 输出一个回车符（0AH）
	MOV AH,02H									
	INT 21H
	MOV DL,0DH									; 输出一个换行符（0AD）
	MOV AH,02H									
	INT 21H
    INC X										; 准备打印下一行, X+=1
	MOV Y,0										; 复位Y, 使回到新行的第一位
    CMP X,15									; 如果棋盘没有全部打印完成
	JNE LOOP2									; 跳转至循环部分继续打印
    MOV DX,OFFSET CLEANBOARD					; 否则清空棋盘
    MOV AH,09H									
    INT 21H
    MOV AH,02H									; 使用INT 10H(BIOS中断)的设置光标位置功能:DH=行, DL=列
	MOV DL,00H									; 光标从10行 0列开始
    MOV DH,10H									
	INT 10H
	POP DX										; 恢复CPU现场
	POP AX
	POP SI
	RET										    ; 子程序结束返回
PRINT ENDP 
; =========/*播放音乐*/========
GENSOUND PROC NEAR
     PUSH AX            						; 保存CPU现场
     PUSH BX
     PUSH CX
     PUSH DX
     PUSH DI

     MOV AL, 0B6H								; 初始化8253
     OUT 43H, AL
     MOV DX, 12H
     MOV AX, 348CH
     DIV DI
     OUT 42H, AL

     MOV AL, AH
     OUT 42H, AL

     IN AL, 61H
     MOV AH, AL									; PB0和PB1同时置为1以驱动扬声器
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

     POP DI            							; 恢复CPU现场
     POP DX
     POP CX
     POP BX
     POP AX
     RET 
GENSOUND ENDP

WAITF PROC NEAR
	PUSH AX            							; 保存CPU现场
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
; =======/*发声程序*/=========
MUSIC PROC NEAR
    MOV AX, DATA
    MOV DS, AX
	CMP MUS_TYPE, 1								; 判断音乐类型
	JE MUS_T1
	CMP MUS_TYPE, 2
	JE MUS_T2
	ADDRESS MUS_FREQ0, MUS_TIME0				; 取音乐0的地址
	JMP MUSSTT
MUS_T1:
	ADDRESS MUS_FREQ1, MUS_TIME1				; 取音乐1的地址
	JMP MUSSTT
MUS_T2:											; 取音乐2的地址
	ADDRESS MUS_FREQ2, MUS_TIME2
MUSSTT:											; 初始化完成, 开始播放工作
    XOR AX, AX									; 清零
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
; =======/*自定义打印程序*/=========
; 输入参数: BL为打印的属性
; 输入参数: DL为打印的字符
MYOUTPUT PROC NEAR							
	PUSH AX
	PUSH CX
	MOV AH, 09H									; 10H中断;  功能号09实现输出
	MOV AL, DL									; 内容
	MOV BL, BL									; 属性: IRGB|IRGB, 前为背景、后为字体;  I表示高亮
	MOV BH, 0									; 页号
	MOV CX, 1									; 打印次数
	INT 10H										
	MOV AH, 03H									; 10H中断;  功能号03获取光标坐标
	INT 10H										
	INC DL										; 光标后移1位打印下一个字符
	MOV AH, 02H									; 10H中断;  功能号02设置光标坐标
	INT 10H
	POP CX
	POP AX
	RET
MYOUTPUT ENDP

CODE ENDS
	END START	
