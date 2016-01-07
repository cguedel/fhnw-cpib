package ch.fhnw.cpib.vm;

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
		if (this.isPeriodic()){
		return this.getInteger() + "." + this.decimalpoints() /*+ "(" + this.periodicpoints() + ")"+*/;
		}
		else{
		return this.getRatioFormat() +"";
		}
		
	}
	
	private int periodicpoints() {
		
		return 0;
	}

	// Helping Stuff to find out if a Ratio is periodic
	public boolean isPeriodic (){
		Ratio check = this.getCanceled();
		int period = check.denominator;
		while (period%2 == 0){
			period = period/2;
		}
		while (period%5 == 0){
			period = period/5;			
		}
		return (!(period == 0));
	}
	
    public double getRatioFormat(){		
		double n = ((double)this.numerator/this.denominator);
		return n;
	};
	
	public int getInteger(){
		int n = (this.numerator/this.denominator);
		return n;
	}
	
	// Returns the not Periodic Part of the Floating Point Number
	public String decimalpoints(){
		int count2 = 0;
		int count5 = 0;
		int denom;
		int num;
		
		//I think only Jack Bauer knows what im doing here, and me... so I'm as good as Jack ;-)
		
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
		
		if (count2 > count5){
			basis = 5;
		} else {
			basis = 2;
		}
		int extd = (int)(Math.pow(basis, exponent));
	//	System.out.println("Basis: " + basis);
	//	System.out.println("Exponent: " + exponent);
	//	System.out.println("#2: " + count2);
	//	System.out.println("#5: " + count5);
	//	System.out.println("Erweiterung: " + extd);
				
		int nnum = extd*num;
		int ndenom = extd*(this.denominator);
	//	System.out.println("Bruch: " + nnum + "/" + ndenom);
		
		int count10 = 0;
		
		while (ndenom%10 == 0){
			ndenom = ndenom/10;
			count10++;			
		}
	//	System.out.println("#0: " + count10);
				
		int decimalnum = nnum/ndenom;
		int decimaldenom = (int)Math.pow(10, count10);
		
	//	System.out.println("Dezimalteilbruch: " + decimalnum + "/" + decimaldenom);
		
		Ratio decimal = new Ratio(decimalnum, decimaldenom);
		
		String str = decimal.getRatioFormat()+"";
		
	//	System.out.println(str);
				
		str = (str.replaceFirst("0", "")).replace(".", "");
		
		if (str.matches("0"))
			str = "Jack";
				
	//	System.out.println(str);
		
		return str;
		
	}
	

	
	
	// End of Helping Stuff

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