package ch.fhnw.cpib.vm;

import java.lang.reflect.Array;
import java.util.Arrays;

public class Ratio {
	private int numerator;
	private int denominator;

	public Ratio(int numerator, int denominator) {
		this.numerator = numerator;
		this.denominator = denominator;
	}

	public int getNumerator() {
		return numerator;
	}

	public int getDenominator() {
		return denominator;
	}

	public String toString() {
		
		Ratio check = this.getCanceled();
		
		if (check.numerator%check.denominator == 0){
		 return check.getInteger() + ".0";	
		} else if (check.isPeriodic()){
		return check.getInteger() + "." + check.decimalpoints();
		 }else {
		return check.getRatioFormat() +"";
		}
		
	}
	
	private int periodicpoints() {
		
		return 0;
	}

	// Helping Stuff to find out if a Ratio is periodic
	public boolean isPeriodic (){
		Ratio check = this.getCanceled();
		int period = check.denominator;
		
		while ((period%2 == 0) && (period > 2)){
			period = period/2;
			//System.out.println(period);
		}
		
		while ((period%5 == 0) && (period > 5)){
			period = period/5;
			//System.out.println(period);
		}
		
		if (period%2 == 0 || period%5 == 0)
			return false;
		//System.out.println(period);
		return (period != 0);
		
		
	}
	
    public double getRatioFormat(){		
		double n = ((double)this.numerator/this.denominator);
		return n;
	};
	
	public int getInteger(){
		int n = (this.numerator/this.denominator);
		return n;
	}
	
	public String decimalpoints(){
		int count2 = 0;
		int count5 = 0;
		int denom;
		int num;
		
		
		int RatioIsBiggerThanOne = (int)(this.getRatioFormat());
		
		if(this.numerator>this.denominator){
			Ratio ratioBiggerThanOne = new Ratio(RatioIsBiggerThanOne,1);
			Ratio small = sub(this, ratioBiggerThanOne);
			denom = small.denominator;
			num = small.numerator;
		} else {
			denom = this.denominator;
			num = this.numerator;
		}
		
		while (denom%2 == 0){
			denom = denom/2;
			count2++;
		}
		
		while (denom%5 == 0){
			denom = denom/5;
			count5++;			
		}
		int basis;
		int exponent = (int)(Math.abs(count2-count5));
		
		//System.out.println(count2);
		//System.out.println(count5);
		
		if (count2 > count5){
			basis = 5;
		} else {
			basis = 2;
		}
		int extd = (int)(Math.pow(basis, exponent));
				
		int nnum = extd*num;
		int ndenom = extd*(this.denominator);
		
		int count10 = 0;
		
		while (ndenom%10 == 0){
			ndenom = ndenom/10;
			count10++;
		}
				
		int decimalnum = nnum/ndenom;
		int decimaldenom = (int)Math.pow(10, count10);
		
		//System.out.println(count10);
		
		
		Ratio decimal = new Ratio(decimalnum, decimaldenom);
		
		//System.out.println("Bruch dezimal: " + decimalnum + "/" + decimaldenom);
		
		String str = decimal.getRatioFormat() +"";
		str = (str.replaceFirst("0", "")).replace(".", "");
		
		int periodicnum = nnum%ndenom;
		int periodicdenom = (int)(ndenom*(Math.pow(10, count10)));
				
		int pnum = periodicnum;
	    int pdenom = periodicdenom/decimaldenom;
	    
	    int plenghth = 0;
	    int counter = 0;
	    pnum=10;
	     
	    
	    while (plenghth != 1){
	    	plenghth = (pnum%pdenom);
	    	pnum = plenghth*10;
	    	counter++;
	    }
	    
	    pnum = periodicnum;
	    int[] numbers = new int[counter+1]; 
	    
	    int anum= 0;
	    int cnt = 0;
	    int tmp = 0;
	    int rest = 0;
	    
	    for (int i = 0; i < numbers.length; i++) {
			
		
	    	anum = pnum/pdenom;
	    	
	    	numbers[cnt] = anum;
	        cnt++;
	    	tmp = anum*pdenom;
	    	
	    	rest = (pnum-tmp);
	    	
	    	pnum = (rest*10);
	    	
	    	}
	    
	    StringBuffer strBuf = new StringBuffer();
	    for (int i = 1; i < numbers.length; i++) {
	    	 strBuf.append(numbers[i]);
		}
	    
	    String finalString = strBuf.toString();
	    
	    
	    int decimaln = (count10);
	    int decimals[] = new int[decimaln];
	    for (int i = 0; i < decimals.length; i++) {
			decimals[i]=0;
		}
	    StringBuffer strBuf1 = new StringBuffer();
	    for (int i = 0; i < decimals.length; i++) {
	    	 strBuf1.append(decimals[i]);
		}
	    
	    if ((decimalnum == 0) && str.equals("0")){
	    str = strBuf1.toString();
	    //System.out.println(str);
	    	return str +  "(" + finalString + ")";
	    }else if (str.equals("0")){
	    	return "(" + finalString + ")";
	    } else {
	    	return str +  "(" + finalString + ")";
	    }
	    
	    
	}
	        

	public Ratio getCanceled() {
		int gcd = getGcd(this.numerator, this.denominator);
		return new Ratio(this.numerator / gcd, this.denominator / gcd);
	}

	public int getFloored() {
		return (int) Math.floor((double) this.numerator
				/ (double) this.denominator);
	}

	public int getCeiled() {
		return (int) Math.ceil((double) this.numerator
				/ (double) this.denominator);
	}

	public int getRounded() {
		return (int) Math.round((double) this.numerator
				/ (double) this.denominator);
	}

	public static Ratio add(Ratio a, Ratio b) {
		int commonGcd = a.getDenominator() * b.getDenominator();
		return new Ratio(a.getNumerator() * b.getDenominator()
				+ b.getNumerator() * a.getDenominator(), commonGcd);
	}

	public static Ratio sub(Ratio a, Ratio b) {
		int commonGcd = a.getDenominator() * b.getDenominator();
		return new Ratio(a.getNumerator() * b.getDenominator()
				- b.getNumerator() * a.getDenominator(), commonGcd);
	}

	public static Ratio multiply(Ratio a, Ratio b) {
		return new Ratio(a.getNumerator() * b.getNumerator(),
				a.getDenominator() * b.getDenominator());
	}
	
	public static Ratio divTrunc(Ratio a, Ratio b) {
		return new Ratio(a.getNumerator() * b.getDenominator(),
				a.getDenominator() * b.getNumerator());
	}

	public static Ratio inv(Ratio a) {
		return new Ratio(a.getNumerator() * -1, a.getDenominator());
	}

	private static int getGcd(int a, int b) {
		if (b == 0) {
			return a;
		}

		return getGcd(b, a % b);
	}

	public static Ratio fromString(String arg) {
		int separatorPos = arg.indexOf('/');
		int numerator = Integer.parseInt(arg.substring(0, separatorPos));
		int denominator = Integer.parseInt(arg.substring(separatorPos + 1));

		return new Ratio(numerator, denominator);
	}

	public static boolean isSame(Ratio a, Ratio b) {
		Ratio aC = a.getCanceled();
		Ratio bC = b.getCanceled();

		return aC.getNumerator() == bC.getNumerator()
				&& aC.getDenominator() == bC.getDenominator();
	}

	public static boolean isGreater(Ratio a, Ratio b) {
		return a.getNumerator() * b.getDenominator() > b.getNumerator()
				* a.getDenominator();
	}

	public static boolean isSmaller(Ratio a, Ratio b) {
		return a.getNumerator() * b.getDenominator() < b.getNumerator()
				* a.getDenominator();
	}

	public static boolean isGreaterOrEqual(Ratio a, Ratio b) {
		return a.getNumerator() * b.getDenominator() >= b.getNumerator()
				* a.getDenominator();
	}

	public static boolean isSmallerOrEqual(Ratio a, Ratio b) {
		return a.getNumerator() * b.getDenominator() <= b.getNumerator()
				* a.getDenominator();
	}
}