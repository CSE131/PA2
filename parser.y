/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should
 *      accept the language as described in specification, and as augmented
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */

/* yylval
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser.
 *
 * pp2: You will need to add new fields to this union as you add different
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    float floatConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
    
    VarDecl* vardecl;
    List<VarDecl*> *varList;
    
    Type *type;
    TypeQualifier* typeQualifier;
    
    FnDecl *functionDecl;
    
    Expr * expr;
    List<Expr *> *exprList;
    
    Stmt *stmt;
    List<Stmt *> *stmtList;
    
    Identifier * fnidentifier;
    Operator * operator;
    unsigned int uintConstant;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Float
%token   T_LessEqual T_GreaterEqual T_EQ T_NE T_LeftAngle T_RightAngle
%token   T_And T_Or
%token   T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_Const T_Uniform T_Layout T_Continue T_Do
%token   T_Inc T_Dec T_Switch T_Case T_Default
%token   T_In T_Out T_InOut
%token   T_Mat2 T_Mat3 T_Mat4 T_Vec2 T_Vec3 T_Vec4
%token   T_Ivec2 T_Ivec3 T_Ivec4 T_Bvec2 T_Bvec3 T_Bvec4
%token   T_Uint T_Uvec2 T_Uvec3 T_Uvec4 T_Struct
%token   T_Semicolon T_Dot T_Colon T_Question T_Comma
%token   T_Dash T_Plus T_Star T_Slash
%token   T_LeftParen T_RightParen T_LeftBracket T_RightBracket T_LeftBrace T_RightBrace

%token   <identifier> T_Identifier
%token   <integerConstant> T_IntConstant
%token   <floatConstant> T_FloatConstant
%token   <boolConstant> T_BoolConstant
%token   <identifier> T_FieldSelection
%token   <uintConstant> T_UintConstant

/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>        DeclList
%type <decl>            Decl
%type <decl>            ExtDecl
%type <vardecl>         Param_Decl
%type <varList>         Param_Decl_List
%type <type>            TypeSpecifier;
%type <typeQualifier>   Type_Qualifier
%type <functionDecl>    Function_Decl
%type <expr>            PrimaryExpr
%type <expr>            PostfixExpr
%type <expr>            UnaryExpr
%type <expr>            RelationalExpr
%type <expr>            AdditiveExpr
%type <expr>            MultiplyExpr
%type <expr>            FnCallExpr
%type <expr>            FnCallParams
%type <expr>            FnCallNoParams
%type <expr>            EqualExpr
%type <expr>            AndExpr
%type <expr>            OrExpr
%type <expr>            Expression
%type <exprList>        ExprList
%type <stmt>            Statement
%type <stmtList>        StatementList
%type <operator>        AssignmentOp
%type <fnidentifier>    Function_Iden
%type <stmt>            SimpleStatement
%type <stmt>            SelectionStmt
%type <stmt>            JumpStatement
%type <stmt>            IterStatement
%type <stmt>            SwitchStatement
%type <stmt>            CompStatement
%type <stmt>            SwitchCaseStmt
%type <stmt>            ExprStmt

//associativity rules
%nonassoc LOWERELSE
%nonassoc T_Else
%right T_Equal T_MulAssign T_DivAssign T_AddAssign T_SubAssign
%right T_Question T_Colon
%left T_EQ T_NE T_LeftAngle T_RightAngle T_And T_Or T_GreaterEqual T_LessEqual
%left T_Plus T_Dash
%left T_Star T_Slash

%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.

 */

/*----------------------------------------Translation units---------------------------------------------*/
Program         : DeclList  {@1;
                                /* pp2: The @1 is needed to convince
                                 * yacc to set up yylloc. You can remove
                                 * it once you have other uses of @n*/
                                 Program *program = new Program($1);
                                 // if no errors, advance to next phase
                                 if (ReportError::NumErrors() == 0)
                                    program->Print(0);
                            }
                ;

DeclList        : DeclList Decl { ($$=$1)->Append($2); }
                | Decl  { ($$ = new List<Decl*>)->Append($1); }
                ;

Decl            : Function_Decl CompStatement
                    {
                        $$ = $1;
                        if($2) $1->SetFunctionBody($2);
                    }
                | ExtDecl {$$ = $1;}
                ;


/*---------------------------------------------Expressions----------------------------------------------*/
PrimaryExpr     : T_Identifier        {$$ = new VarExpr(@1, new Identifier(@1, $1));}
                | T_IntConstant       {$$ = new IntConstant(@1, $1);}
                | T_FloatConstant     {$$ = new FloatConstant(@1, $1);}
                | T_BoolConstant      {$$ = new BoolConstant(@1, $1);}
                | T_LeftParen Expression T_RightParen     {$$ = $2;}
                ;

PostfixExpr     : PrimaryExpr         {$$ = $1;}
                | PostfixExpr T_LeftBracket Expression T_RightBracket {$$ = new ArrayAccess(@1,$1,$3);}
                | PostfixExpr T_Dot T_FieldSelection  {$$ = new FieldAccess($1, new Identifier(@3, $3));}
                | PostfixExpr T_Inc   {$$ = new PostfixExpr($1, new Operator(@2,"++"));}
                | PostfixExpr T_Dec   {$$ = new PostfixExpr($1, new Operator(@2,"--"));}
                | FnCallExpr          {$$ = $1;}
                ;

ExprList        : ExprList T_Comma Expression {($$=$1)-> Append($3);}
                | Expression {($$ = new List<Expr * >)-> Append($1);}
                ;

FnCallParams    : Function_Iden T_LeftParen ExprList {$$ = new Call(@1,NULL,$1,$3);}
                ;

FnCallNoParams  : Function_Iden T_LeftParen         {$$ = new Call(@1,NULL,$1,new List<Expr *>());}
                | Function_Iden T_LeftParen T_Void  {$$ = new Call(@1,NULL,$1,new List<Expr *>());}
                ;

FnCallExpr      : FnCallParams T_RightParen   {$$ = $1;}
                | FnCallNoParams T_RightParen {$$ = $1;}
                ;

UnaryExpr       : PostfixExpr         {$$ = $1;}
                | T_Inc UnaryExpr     {$$ = new ArithmeticExpr(new Operator(@1,"++"),$2);}
                | T_Dec UnaryExpr     {$$ = new ArithmeticExpr(new Operator(@1,"--"),$2);}
                | T_Dash UnaryExpr    {$$ = new ArithmeticExpr(new Operator(@1,"-"),$2);}
                | T_Plus UnaryExpr    {$$ = new ArithmeticExpr(new Operator(@1,"+"),$2);}
                ;

AdditiveExpr    : MultiplyExpr {$$ = $1;}
                | AdditiveExpr T_Plus MultiplyExpr {$$ = new ArithmeticExpr($1, new Operator(@2,"+"),$3);}
                | AdditiveExpr T_Dash MultiplyExpr {$$ = new ArithmeticExpr($1, new Operator(@2,"-"),$3);}
                ;

MultiplyExpr    : UnaryExpr     {$$ = $1;}
                | MultiplyExpr T_Slash UnaryExpr {$$ = new ArithmeticExpr($1, new Operator(@2,"/"),$3);}
                | MultiplyExpr T_Star UnaryExpr {$$ = new ArithmeticExpr($1, new Operator(@2,"*"),$3);}
                ;

RelationalExpr  : AdditiveExpr {$$ = $1;}
                | RelationalExpr T_LeftAngle AdditiveExpr {$$ = new RelationalExpr($1,new Operator(@2,"<"),$3);}
                | RelationalExpr T_RightAngle AdditiveExpr {$$ = new RelationalExpr($1,new Operator(@2,">"),$3);}
                | RelationalExpr T_LessEqual AdditiveExpr {$$ = new RelationalExpr($1,new Operator(@2,"<="),$3);}
                | RelationalExpr T_GreaterEqual AdditiveExpr {$$ = new RelationalExpr($1,new Operator(@2,">="),$3);}
                ;

EqualExpr       : RelationalExpr {$$ = $1;}
                | EqualExpr T_EQ RelationalExpr {$$ = new EqualityExpr($1, new Operator(@2,"=="),$3);}
                | EqualExpr T_NE RelationalExpr {$$ = new EqualityExpr($1, new Operator(@2,"!="),$3);}
                ;

AndExpr         : EqualExpr {$$ = $1;}
                | AndExpr T_And EqualExpr {$$ = new LogicalExpr($1, new Operator(@2,"&&"),$3);}
                ;

OrExpr          : AndExpr {$$ = $1;}
                | OrExpr T_Or AndExpr {$$ = new LogicalExpr($1, New Operator(@2,"||"),$3);}
                ;

Expression      : OrExpr {$$ = $1;}
                | UnaryExpr AssignmentOp Expression {$$ = new AssignExpr($1,$2,$3);}
                | OrExpr T_Question Expression T_Colon Expression {$$ = new ConditionalExpr($1,$3,$5);}
                ;

AssignmentOp    : T_Equal {$$ = new Operator(@1,"=");}
                | T_MulAssign {$$ = new Operator(@1,"*=");}
                | T_DivAssign {$$ = new Operator(@1,"/=");}
                | T_AddAssign {$$ = new Operator(@1,"+=");}
                | T_SubAssign {$$ = new Operator(@1,"-=");}
                ;

Function_Iden   : T_Identifier {$$ = new Identifier(@1,$1);}
                ;

TypeSpecifier   : T_Void {$$ = Type::voidType;}
                | T_Int {$$ = Type::intType;}
                | T_Float {$$ = Type::floatType;}
                | T_Bool {$$ = Type::boolType;}
                | T_Uint {$$ = Type::uintType;}
                | T_Mat2 {$$ = Type::mat2Type;}
                | T_Mat3 {$$ = Type::mat3type;}
                | T_Mat4 {$$ = Type::mat4Type;}
                | T_Uvec2 {$$ = Type::uvec2Type;}
                | T_Uvec3 {$$ = Type::uvec3Type;}
                | T_Uvec4 {$$ = Type::uvec4Type;}
                | T_Ivec2 {$$ = Type::ivec2Type;}
                | T_Ivec3 {$$ = Type::ivec3Type;}
                | T_Ivec4 {$$ = Type::ivec4Type;}
                | T_Bvec2 {$$ = Type::bvec2Type;}
                | T_Bvec3 {$$ = Type::bvec3Type;}
                | T_Bvec4 {$$ = Type::bvec4Type;}
                | T_Vec2 {$$ = Type::vec2Type;}
                | T_Vec3 {$$ = Type::vec3Type;}
                | T_Vec4 {$$ = Type::vec4Type;}
                ;

/*--------------------------------------------End of Expressions----------------------------------------*/

/*---------------------------------------------Statements-----------------------------------------------*/
SimpleStatement : ExprStmt {$$ = $1;}
                | SelectionStmt {$$ = $1;}
                | SwitchStatement {$$= $1;}
                | SwitchCaseStmt  {$$= $1;}
                | IterStatement {$$ = $1;}
                | JumpStatement {$$ = $1;}
                | Param_Decl  T_Semicolon {$$  = new DeclStmt($1);}
                ;

Statement       : CompStatement {$$ = $1;}
                | SimpleStatement {$$ = $1;}
                ;

CompStatement    : T_LeftBrace T_RightBrace {$$  = new StmtBlock(new List<VarDecl*>(),new List<Stmt*>);}
                | T_LeftBrace  StatementList T_RightBrace {$$ = new StmtBlock(new List<VarDecl*>(),$2);}
                ;

StatementList   : Statement {($$ = new List<Stmt *>)->Append($1);}
                | StatementList Statement {($$=$1)->Append($2); }
                ;

SelectionStmt   : T_If T_LeftParen Expression T_RightParen Statement {$$= new IfStmt($3,$5,NULL);}
                    %prec  LOWERELSE
                | T_If T_LeftParen Expression T_RightParen Statement T_Else Statement {$$= new IfStmt($3,$5,$7);}
                ;

ExprStmt        : T_Semicolon {$$ = new EmptyExpr();}
                | Expression T_Semicolon {$$ = $1;}
                ;

SwitchStatement  : T_Switch T_LeftParen Expression T_RightParen T_LeftBrace StatementList T_RightBrace
                    {$$ = new SwitchStmt($3,$6,NULL);}
                ;

JumpStatement   : T_Break T_Semicolon {$$ = new BreakStmt(@1);}
                | T_Return T_Semicolon {$$ = new ReturnStmt(@1,NULL);}
                | T_Return Expression T_Semicolon {$$= new ReturnStmt(@1,$2);}
                ;

SwitchCaseStmt  : T_Default T_Colon Statement   {$$= new Default($3);}
                | T_Case Expression T_Colon Statement  {$$ = new Case($2,$4);}
                ;

IterStatement   : T_While T_LeftParen Expression T_RightParen Statement  {$$  = new WhileStmt($3,$5);}
                | T_Do Statement T_While T_LeftParen Expression T_RightParen T_Semicolon
                    {$$ = new DoWhileStmt($2,$5);}
                | T_For T_LeftParen ExprStmt ExprStmt Expression T_RightParen Statement {$$ = new ForStmt($3,$4,$5,$7);}
                ;


/*--------------------------------------------End of Statements-----------------------------------------*/

/*---------------------------------------------Declarations---------------------------------------------*/

Type_Qualifier  : T_Const {$$ = TypeQualifier::constTypeQualifier;}
                | T_In {$$ = TypeQualifier::intTypeQualifier;}
                | T_Out {$$ = TypeQualifier::outTypeQualifier;}
                | T_Uniform {$$ = TypeQualifier::uniformTypeQualifier;}
                ;

/* Go through every single possible combination listed in GLSL */
Param_Decl      : TypeSpecifier T_Identifier {$$ = new VarDecl(new Identifier(@2,$2),$1);}
                | Type_Qualifier TypeSpecifier T_Identifier {$$ = new VarDecl(new Identifier(@3,$3),$2,$1);}

                  //only integers are allowed, no Expressions inside brackets
                | TypeSpecifier T_Identifier  T_LeftBracket T_IntConstant T_RightBracket
                    {$$ = new VarDecl(new Identifier(@2,$2),new ArrayType(@1,$1));}
                | Type_Qualifier TypeSpecifier T_Identifier T_LeftBracket T_IntConstant T_RightBracket
                    {$$ = new VarDecl(new Identifier(@3,$3), new Arraytype(@2,$2),$1);}

                | TypeSpecifier T_Identifier T_Equal Expression {$$ = new VarDecl(new Identifier(@2,$2),$1,$4);}
                | Type_Qualifier TypeSpecifier T_Identifier T_Equal Expression
                    {$$ = new VarDecl(new Identifier(@3,$3),$2,$1,$5);}
                ;

ExtDecl         : Param_Decl T_Semicolon {$$ = $1;}
                | Function_Decl T_Semicolon {$$ = $1;}
                | Type_Qualifier T_Identifier T_Semicolon {$$ = new VarDecl(new Identifier(@2,$2),$1);}
                ;

Param_Decl_List : Param_Decl_List T_Comma Param_Decl {($$=$1)-> Append($3);}
                | Param_Decl {($$= new List<VarDecl*>)-> Append($1);}
                ;

/* Recursively go through all possible rules that can be found in GLSL */
Function_Decl   : TypeSpecifier T_Identifier T_LeftParen T_RightParen
                    {$$ = new FnDecl(new Identifier(@2,$2),$1,new List<VarDecl*>());}
                | TypeSpecifier T_Identifier T_LeftParen Param_Decl_List T_RightParen
                    {$$ = new FnDecl(new Identifier(@2,$2),$1,$4);}
                ;

/*-------------------------------------------End of Declarations---------------------------------------*/




%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
