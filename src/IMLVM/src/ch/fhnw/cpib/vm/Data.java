// Virtual Machine Java 2015, V01
// Edgar F.A. Lederer, FHNW and Uni Basel, 2015

package ch.fhnw.cpib.vm;

public class Data
{
    static interface IBaseData
    {
        IBaseData copy();
    }

    static class IntData implements IBaseData
    {
        private int i;
        IntData(int i) { this.i= i; }
        int getData() { return i; }
        public IntData copy() { return intCopy(this); }
    }

    static IntData intNew(int i)
    {
        return new IntData(i);
    }

    static int intGet(IBaseData a)
    {
        return ((IntData)a).getData();
    }

    static IntData intCopy(IBaseData a)
    {
        return intNew(intGet(a));
    }
    
    static class RatioData implements IBaseData
    {
    	private Ratio r;
    	RatioData(Ratio r) { this.r = r; }
    	Ratio getData() { return r; }
    	public RatioData copy() { return ratioCopy(this); }
    }
    
    static RatioData ratioNew(Ratio r)
    {
    	return new RatioData(r);
    }
    
    static Ratio ratioGet(IBaseData a)
    {
    	if (a instanceof IntData)
    	{
    		return new Ratio(((IntData) a).getData(), 1);
    	}
    	
    	return ((RatioData)a).getData().getCanceled();
    }
    
    static RatioData ratioCopy(IBaseData a)
    {
    	return ratioNew(ratioGet(a));
    }
    
    static IntData ratioNum(IBaseData a)
    {
    	return intNew(ratioGet(a).getNumerator());
    }
    
    static IntData ratioDenom(IBaseData a)
    {
    	return intNew(ratioGet(a).getDenominator());
    }
    
    static IntData ratioFloor(IBaseData a)
    {
    	return intNew(ratioGet(a).getFloored());
    }
    
    static IntData ratioCeil(IBaseData a)
    {
    	return intNew(ratioGet(a).getCeiled());
    }
    
    static IntData ratioRound(IBaseData a)
    {
    	return intNew(ratioGet(a).getRounded());
    }

    // coding booleans as integers
    static IntData boolNew(boolean b)
    {
        return intNew(b ? 1 : 0);
    }

    // coding booleans as integers
    static boolean boolGet(IBaseData a)
    {
        return ((IntData)a).getData() == 1;
    }

    static class FloatData implements IBaseData
    {
        private float f;
        FloatData(float f) { this.f= f; }
        float getData() { return f; }
        public FloatData copy() { return floatCopy(this); }
    }

    static FloatData floatNew(float f)
    {
        return new FloatData(f);
    }

    static float floatGet(IBaseData a)
    {
        return ((FloatData)a).getData();
    }

    static FloatData floatCopy(IBaseData a)
    {
        return floatNew(floatGet(a));
    }

    static IntData intInv(IBaseData a)
    {
        return intNew(-intGet(a));
    }

    static FloatData floatInv(IBaseData a)
    {
        return floatNew(-floatGet(a));
    }
    
    static IntData boolInv(IBaseData a)
    {
    	return boolNew(!boolGet(a));
    }
    
    static RatioData ratioInv(IBaseData a)
    {
    	return ratioNew(Ratio.inv(ratioGet(a)));
    }

    static IntData intAdd(IBaseData a, IBaseData b)
    {
        return intNew(intGet(a) + intGet(b));
    }
    
    static RatioData ratioAdd(IBaseData a, IBaseData b)
    {
    	return ratioNew(Ratio.add(ratioGet(a), ratioGet(b)));
    }

    static IntData intSub(IBaseData a, IBaseData b)
    {
        return intNew(intGet(a) - intGet(b));
    }
    
    static RatioData ratioSub(IBaseData a, IBaseData b)
    {
    	return ratioNew(Ratio.sub(ratioGet(a), ratioGet(b)));
    }

    static IntData intMult(IBaseData a, IBaseData b)
    {
        return intNew(intGet(a) * intGet(b));
    }
    
    static RatioData ratioMult(IBaseData a, IBaseData b)
    {
        return ratioNew(Ratio.multiply(ratioGet(a), ratioGet(b)));
    }

    static IntData intDivTrunc(IBaseData a, IBaseData b) throws IVirtualMachine.ExecutionError
    {
        try
        {
            return intNew(intGet(a) / intGet(b));
        }
        catch (ArithmeticException e)
        {
            throw new VirtualMachine.ExecutionError("Integer division by zero.");
        }
    }
    
    static RatioData ratioDivTrunc(IBaseData a, IBaseData b) throws IVirtualMachine.ExecutionError
    {
    	return ratioNew(Ratio.divTrunc(ratioGet(a), ratioGet(b)));
    }

    static IntData intModTrunc(IBaseData a, IBaseData b) throws IVirtualMachine.ExecutionError
    {
        try
        {
            return intNew(intGet(a) % intGet(b));
        }
        catch (ArithmeticException e)
        {
            throw new VirtualMachine.ExecutionError("Integer remainder by zero.");
        }
    }
    
    static IntData intEQ(IBaseData a, IBaseData b)
    {
        return boolNew(intGet(a) == intGet(b));
    }
    
    static IntData ratioEQ(IBaseData a, IBaseData b)
    {
    	return boolNew(Ratio.isSame(ratioGet(a), ratioGet(b)));
    }

    static IntData intNE(IBaseData a, IBaseData b)
    {
        return boolNew(intGet(a) != intGet(b));
    }
    
    static IntData ratioNE(IBaseData a, IBaseData b)
    {
    	return boolNew(!Ratio.isSame(ratioGet(a), ratioGet(b)));
    }

    static IntData intGT(IBaseData a, IBaseData b)
    {
        return boolNew(intGet(a) > intGet(b));
    }
    
    static IntData ratioGT(IBaseData a, IBaseData b)
    {
    	return boolNew(Ratio.isGreater(ratioGet(a), ratioGet(b)));
    }

    static IntData intLT(IBaseData a, IBaseData b)
    {
        return boolNew(intGet(a) < intGet(b));
    }
    
    static IntData ratioLT(IBaseData a, IBaseData b)
    {
    	return boolNew(Ratio.isSmaller(ratioGet(a), ratioGet(b)));
    }

    static IntData intGE(IBaseData a, IBaseData b)
    {
        return boolNew(intGet(a) >= intGet(b));
    }
    
    static IntData ratioGE(IBaseData a, IBaseData b)
    {
    	return boolNew(Ratio.isGreaterOrEqual(ratioGet(a), ratioGet(b)));
    }

    static IntData intLE(IBaseData a, IBaseData b)
    {
        return boolNew(intGet(a) <= intGet(b));
    }
    
    static IntData ratioLE(IBaseData a, IBaseData b)
    {
    	return boolNew(Ratio.isSmallerOrEqual(ratioGet(a), ratioGet(b)));
    }
}
