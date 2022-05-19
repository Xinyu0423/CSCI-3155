package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) =>{
            val v1=evalExpr(l,env);
            v1 match {
                case NumValue(f)=>{
                    val points=List((0.0,0.0),(f,0.0));
                    val polygon=new Polygon(points);
                    val polygonList=List(polygon);
                    val newCanvas=new MyCanvas(polygonList);
                    val tempFigValue=FigValue(newCanvas);
                    return tempFigValue;
                }
                case _=>throw new IllegalArgumentException();
            }
        }//TODO: Handle a line object
        case EquiTriangle(sideLength) => {
            val v1=evalExpr(sideLength,env);
            v1 match {
                case NumValue(f)=>{
                    val points=List((0.0,0.0),(f, 0.0),(f/2,(math.sqrt(3)*f)/2))
                    val polygon=new Polygon(points);
                    val polygonList=List(polygon);
                    val newCanvas=new MyCanvas(polygonList);
                    val tempFigValue=FigValue(newCanvas);
                    return tempFigValue;
                }
                case _=>throw new IllegalArgumentException;
            }
        } // TODO: Handle Equilateral Triangle
        case Rectangle(sideLength) => {
            val v1=evalExpr(sideLength,env);
            v1 match {
                case NumValue(f)=>{
                    val points=List((0.0,0.0),(0.0,f),(f,f),(f,0.0));
                    val polygon=new Polygon(points);
                    val polygonList=List(polygon);
                    val newCanvas=new MyCanvas(polygonList);
                    val tempFigValue=FigValue(newCanvas);
                    return tempFigValue;
                }
                case _=>throw new IllegalArgumentException;
            }
        } // TODO: Handle square given the side length
        case Circle(rad) => {
            val v1=evalExpr(rad,env)
            v1 match {
                case NumValue(f)=>{
                    val tempCircle=new MyCircle((f,f),f);
                    val circleList=List(tempCircle);
                    val newCanvas=new MyCanvas(circleList);
                    val tempFigValue=FigValue(newCanvas);
                    return tempFigValue;
                }
                case _=>throw new IllegalArgumentException;
            }
        } //TODO: Handle circle
        case Plus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.plus) // TODO: Handle addition of numbers or figures
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.multiplication) // TODO: Handle multiplication of numbers or figures
        case Div(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.division) // TODO: Handle division
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => Closure(x,e,env) //TODO: Handle function definitions
        //Get this from Assignment 6
        case LetRec(f, x, e1, e2) => {
            val env2=ExtendREC(f,x,e1,env);
            return evalExpr(e2,env2);
        } // TODO: Handle recursive functions -- look at Environment.scala
        //Get this from Assignment 6
        case FunCall(fCallExpr, arg) => {
            val v1=evalExpr(fCallExpr,env);
            val v2=evalExpr(arg,env);
            v1 match {
                case Closure(x,closure_ex,closed_env)=>{
                    val new_env=Extend(x,v2,closed_env);
                    return evalExpr(closure_ex,new_env);
                }
                case _=>{
                    throw new IllegalArgumentException();
                }
            }
        } // TODO: Handle function calls
        //Get this from Assignment 6
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
