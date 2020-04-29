#define _CRT_SECURE_NO_WARNINGS

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>

const int BUFF_LEN = 50;
const int REAL_ACCURACY = 1;// 小数点后位数

int gExprN = 50;			// 默认50个式子
int gEnableReal;			// 是否允许小数
int gUpperBound = 50;		// 上下界
int gLowerBound = 1;
int gEnableParen = 1;
int gSaveFile = 0;			// 是否保存
const char *gFileName = "./out.txt";
FILE *gOStream = stdout;

enum _OperatorType
{
	OT_ADD, OT_SUB,
	OT_MUL, OT_DIV,
	OPERATOR_TYPE_N = 4,	// 一共几种算符
	OT_BAD
};
struct _Operator			// 算符的优先级与启用状态
{
	enum _OperatorType ot;
	const char *str;
	char priority;			// 之前的优先级已经没用了
	int enable;
} gOperatorTable[] = {
	{	OT_ADD,	"+",	15,	1},
	{	OT_SUB,	"-",	15, 1},
	{	OT_MUL,	"*",	16, 1},
	{	OT_DIV,	"/",	16, 1}
};
enum _ValueType
{
	VT_INTEGER,
	VT_REAL,
	VALUE_TYPE_N = 2,		// 一共有几种值类型
	VT_BAD
};
struct _Expr
{
	struct _Expr *expLeft;
	struct _Expr *expRight;
	struct _Operator *op;
	double value;
	enum _ValueType vt;
};
// 叶节点（表达式中的数字）的个数==maxNVal
void BuildTree(struct _Expr *&exp, int maxNVal)
{
	int nL, t;
	if (maxNVal < 1)
		return;
	exp = (struct _Expr *)malloc(sizeof(struct _Expr));
	if (exp == NULL)
		return;
	exp->op = NULL;
	exp->vt = VT_BAD;
	if (maxNVal == 1)
	{
		exp->expLeft = NULL;
		exp->expRight = NULL;
		return;
	}
	nL = ((t = rand() % maxNVal) == 0) ? 1 : t;
	BuildTree(exp->expLeft, nL);
	BuildTree(exp->expRight, maxNVal - nL);
	if (exp->expLeft == NULL || exp->expRight == NULL)
	{
		free(exp);	// 要么全部节点构建成功，要么释放所有节点内存。
		exp = NULL;
	}
}
// 烦人的括号规则， 一言难尽
const unsigned char gBindingTableL[OPERATOR_TYPE_N][OPERATOR_TYPE_N] = {
	{0,0,1,1},{0,0,1,1},{0,0,0,0},{0,0,0,0}
};
const unsigned char gBindingTableR[OPERATOR_TYPE_N][OPERATOR_TYPE_N] = {
	{0,0,0,0},{1,1,0,0},{1,1,0,0},{1,1,1,1}
};
// 从gOperatorTable中随机选择可用算符
int auxPickOp()
{
	int i = 0;
	while (1)
	{
		if (gOperatorTable[i].enable && rand()%2)
			break;
		i = (i + 1) % OPERATOR_TYPE_N;
	}
	return i;
}
// 使用之前请确保gOperatorTable至少有一个算符可用
void FillOperator(struct _Expr *exp)
{
	if (!exp || !exp->expLeft && !exp->expRight)
		return;
	exp->op = gOperatorTable + auxPickOp();
	FillOperator(exp->expLeft);
	FillOperator(exp->expRight);
}
// 求值。用于后面判断除数是否为零
void Calculate(struct _Expr *exp)
{
	if (!exp->expLeft && !exp->expRight)
		return;
	Calculate(exp->expLeft);
	Calculate(exp->expRight);
	switch (exp->op->ot)
	{
	case OT_ADD:
		exp->value = exp->expLeft->value + exp->expRight->value;
		break;
	case OT_SUB:
		exp->value = exp->expLeft->value - exp->expRight->value;
		break;
	case OT_MUL:
		exp->value = exp->expLeft->value * exp->expRight->value;
		break;
	case OT_DIV:
		_ASSERT(exp->expRight->value != 0);
		exp->value = exp->expLeft->value / exp->expRight->value;
		break;
	default:
		break;
	}
}
// 为叶节点填充随机数字
void FillValue(struct _Expr *exp)
{
	if (exp == NULL)
		return;
	if (exp->expLeft == NULL && NULL == exp->expRight)
	{
FILL_VALUE_LP6Y:
		int i = rand() % VALUE_TYPE_N;
		switch (i)
		{
		case VT_INTEGER:
			exp->value = rand() % (gUpperBound - gLowerBound) + gLowerBound;
			exp->vt = VT_INTEGER;
			break;
		case VT_REAL:
			{
				if (gEnableReal == 0)
					goto FILL_VALUE_LP6Y;
				exp->value = rand() % (gUpperBound - gLowerBound) + gLowerBound;
				i = (int)pow(10, REAL_ACCURACY);
				exp->value += (rand() % i) / (double)i;
				exp->vt = VT_REAL;
			}
			break;
		default:
			exp->vt = VT_BAD;
			break;
		}
	}
	// 被除数是零则重新为节点赋值
FILL_VALUE_LP_TR2:
	FillValue(exp->expRight);
	if (exp->op != NULL && exp->op->ot == OT_DIV)
	{
		Calculate(exp->expRight);
		if (exp->expRight->value == 0)
			goto FILL_VALUE_LP_TR2;
	}
	FillValue(exp->expLeft);
}
// 将树的结构和内容转换成字符串
int ExprToString(struct _Expr *exp, char *exprStr, int size)
{
	if (exp == NULL || size < 1)
		return 0;
	int pos = 0;
	if (exp->expLeft == NULL && exp->expRight == NULL)
	{
		switch (exp->vt)
		{
		case VT_INTEGER:
			pos = snprintf(exprStr, BUFF_LEN, exp->value < 0 ? "(%d)" : "%d", (int)exp->value);
			break;
		case VT_REAL:
			pos = snprintf(exprStr, BUFF_LEN, exp->value < 0 ? "(%.*f)" : "%.*f", REAL_ACCURACY, exp->value);
			break;
		default:
			break;
		}
		return pos;
	}
	// 也许当初的起点就是错的
	// 现在为了正确显示括号我居然要查表，太令人无语了
	if (exp->expLeft->op && gBindingTableL[exp->expLeft->op->ot][exp->op->ot])
	{
		exprStr[pos++] = '(';
		pos += ExprToString(exp->expLeft, exprStr + pos, size - pos);
		exprStr[pos++] = ')';
	}
	else
		pos += ExprToString(exp->expLeft, exprStr + pos, size - pos);
	pos += snprintf(exprStr + pos, BUFF_LEN, exp->op->str);
	if (exp->expRight->op && gBindingTableR[exp->op->ot][exp->expRight->op->ot])
	{
		exprStr[pos++] = '(';
		pos += ExprToString(exp->expRight, exprStr + pos, size - pos);
		exprStr[pos++] = ')';
	}
	else
		pos += ExprToString(exp->expRight, exprStr + pos, size - pos);
	return pos;
}
void DestroyTree(struct _Expr *exp)
{
	if (exp)
	{
		DestroyTree(exp->expLeft);
		DestroyTree(exp->expRight);
		free(exp);
		exp = NULL;
	}
}
// 工具，接收整数
void InputInteger(int *x, int prefer)
{
	int n, i;
	char buff[BUFF_LEN], *pBuff;
INPUT_INT_LP_A0:
	n = prefer;
	if (!fgets(buff, BUFF_LEN, stdin))
		exit(-1);
	i = 0;
	while (buff[i] != 0 && isspace(buff[i]))
		i++;
	if (buff[i] != 0)
	{
		if (strcmp(buff + i, "q") == 0)
			exit(0);
		n = (int)strtol(buff + i, &pBuff, 10);
		if (pBuff == buff + i)
		{
			printf("Invalid parameters! Please input again:");
			goto INPUT_INT_LP_A0;
		}
	}
	*x = n;
}
// 工具，选择是否
void ChooseYesOrNo(int *x, int prefer)
{
	int i, choice = prefer;
	char buff[BUFF_LEN];
	if (!fgets(buff, BUFF_LEN, stdin))
		exit(-1);
	i = 0;
	while (buff[i] != 0 && isspace(buff[i]))
		i++;
	if (buff[i] != 0)
	{
		if (strcmp(buff + i, "q") == 0)
			exit(0);
		if (tolower(buff[i]) == 'y' && buff[i + 1] == '\n')
			choice = 1;
		else if (tolower(buff[i]) == 'n' && buff[i + 1] == '\n')
			choice = 0;
	}
	*x = choice;
}
// 指定表达式个数
void InputExprN()
{
	int temp = gExprN;
	while (1)
	{
		printf("How many expressions do you want(default=%d)?\n", gExprN);
		InputInteger(&temp, gExprN);
		if (temp <= 500)
			break;
		printf("\n\t\033[31mToo large! You'd better adjust to a smaller value.\033[0m\n\n");
	}
	gExprN = temp;
}
/*
 * 等待用户指定随机数的范围
 * 按‘q’退出并返回-1；最小值小于最大值则返回0，不满足此条件则返回-1；
 * 输入“3.1415”时将得到数字“3”，虽然按道理来讲小数更好但是作业无此要求。
 */
void SetBound()
{
	int high;
SET_BD_LP_00:
	printf("Set the upper bound of the numbers(default=%d):", gUpperBound);
	InputInteger(&high, gUpperBound);
	if (1 >= high)
	{
		printf("\n\033[31mThe upper bound must be greater than 1!Please input again.\033[0m\n\n");
		goto SET_BD_LP_00;
	}
	gUpperBound = high;
}
// 等待用户依次选择是否启用各个算符
int SelectOperators()
{
	int i, j;
	char *pBuff, buff[BUFF_LEN];
	printf("\n\t\t\033[32mChoose operators\n");
	printf("Input \"q\" to quit and press <Enter> to keep default.\033[0m\n\n");
	i = 0;
LP_SELECT_OP_0:
	printf("Enable operator \"%s\"?(%s):",
		gOperatorTable[i].str,
		gOperatorTable[i].enable == 1 ? "Y/n" : "y/N"
	);
	if (!fgets(buff, BUFF_LEN, stdin))
		return -1;
	pBuff = buff;
	while (*pBuff && isspace(*pBuff))		// 跳过空白
		pBuff++;
	if (*pBuff != 0)
	{
		if (*pBuff=='q' && *(pBuff+1)=='\n')
			exit(0);
		// 此时 pBuff + 1 一定存在！
		if (tolower(*pBuff) == 'n' && *(pBuff + 1) == '\n')
		{
			gOperatorTable[i].enable = 0;
		}
		else if (tolower(*pBuff) == 'y' && *(pBuff + 1) == '\n')
		{
			gOperatorTable[i].enable = 1;
		}
	}
	i++;
	if (i < OPERATOR_TYPE_N)
		goto LP_SELECT_OP_0;
	i = 0;
	for (j = 0; j < OPERATOR_TYPE_N; j++)	// 统计被启用的算符个数
	{
		if (gOperatorTable[j].enable == 1)
			i++;
	}
	if (i == 0)
	{
		printf("\n\t\033[31mYou MUST choose at least one！Please input again.\033[0m\n\n");
		goto LP_SELECT_OP_0;
	}
	return 0;
}
void SetRandomSeed()
{
	unsigned int variable;					// 不要初始化！
	unsigned int *address = &variable;
	srand((unsigned int)(address) ^ variable);
	srand(rand() | (1<<15));
}
int PrintStr(const char *fmt, ...)
{
	char fm_buffer[128];
	int fm_len = 0;
	va_list ap;
	va_start(ap, fmt);
	fm_len = vsnprintf(fm_buffer, 128, fmt, ap);
	va_end(ap);
	return fprintf(gOStream, fm_buffer);
}
//////////////////////////////////////////////////////////////////////
// 一旦禁用括号，上面的树形结构就很难处理。但它能正常工作，我不敢再改了//
// 因为树的结构蕴含了括号，所以若禁用它我就要在构建时对树的结构加以限制//
// 反正我在仅剩的五个小时内想不到好办法，所以我花10分钟写了下面这段代码//
//////////////////////////////////////////////////////////////////////
int pStr = 0;
int pos = 0;
char strOfExpr[BUFF_LEN << 1];
char opInExpr[BUFF_LEN];
char valueTypes[BUFF_LEN];
double valInExpr[BUFF_LEN];

double pickANumber(char type)
{
	double a, b;
	a = rand() % (gUpperBound)+1;
	if (type == 2)
	{
		b = pow(10, REAL_ACCURACY);
		a += (double)(rand() % (int)b) / b;
	}
	return a;
}
void Gen(int n)
{
	if (n < 1)
		return;
	int type, i;
	pos = 0;
	for (i = 0; i < n; i++)
	{
		type = 1;
		if (gEnableReal)
			type += rand() % 2;
		valInExpr[pos] = pickANumber(type);
		valueTypes[pos] = type;
		opInExpr[pos] = gOperatorTable[auxPickOp()].str[0];
		while (opInExpr[pos] == '/' && valInExpr[pos] == 0)
				valInExpr[pos] = pickANumber(type);
		pos++;
	}
}
int PrintVal(int i, char *str, int max)
{
	int ii;
	if (valueTypes[i] - 1)
		ii = snprintf(str, max, "%.*f", REAL_ACCURACY, valInExpr[i]);
	else
		ii = snprintf(str, max, "%d", (int)valInExpr[i]);
	return ii;
}
int GetStrOfExpr()
{
	if (pos < 2)
		return 0;
	int i, j;
	j = 0;
	memset(strOfExpr, 0, BUFF_LEN);
	j += PrintVal(0, strOfExpr + j, BUFF_LEN);
	for (i = 1; i < pos; i++)
	{
		j += snprintf(strOfExpr + j, BUFF_LEN, "%c", opInExpr[i-1]);
		j += PrintVal(i, strOfExpr + j, BUFF_LEN);
	}
	return j;
}
//////////////////////////////////////////////////////////////////////

int main()
{
	struct _Expr *exp;

	SetRandomSeed();
	InputExprN(&gExprN);
	SelectOperators();
	SetBound();
	printf("Do you want parentheses in the expression?(Y/n):");
	ChooseYesOrNo(&gEnableParen, 1);
	printf("Do you want some float numbers?(Y/n):");
	ChooseYesOrNo(&gEnableReal, 1);
	printf("Output to file?(y/N):");
	ChooseYesOrNo(&gSaveFile, 0);
	if (gSaveFile)
	{
		gOStream = fopen(gFileName, "w+");
		if (!gOStream)
			exit(-1);
	}
	for (int i = 0; i < gExprN; i++)
	{
		if (gEnableParen)
		{
			memset(strOfExpr, 0, BUFF_LEN);
			BuildTree(exp, rand() % 5 + 2);	// 每个式子将含有2到6个数字
			FillOperator(exp);
			FillValue(exp);
			ExprToString(exp, strOfExpr, BUFF_LEN);
			PrintStr("%30s=____", strOfExpr);
			DestroyTree(exp);
		}
		else
		{
			Gen(rand() % 5 + 2);
			GetStrOfExpr();
			PrintStr("%30s=____", strOfExpr);
		}
		if (i & 1)
			PrintStr("\n");
	}
	if (gSaveFile)
	{
		printf("\r\n\033[32mThe contents of the file are as follows:\033[0m\r\n\r\n");
		rewind(gOStream);
		for (int i = 0; (i = fgetc(gOStream)) != EOF; )
			putchar(i);
		fclose(gOStream);
	}
	return 0;
}
