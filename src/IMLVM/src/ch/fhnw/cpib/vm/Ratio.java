package ch.fhnw.cpib.vm;

public class Ratio {
	private int numerator;
	private int denominator;
	
	public Ratio(int numerator, int denominator)
	{
		this.numerator = numerator;
		this.denominator = denominator;
	}
	
	public int getNumerator() {
		return numerator;
	}

	public int getDenominator() {
		return denominator;
	}

	public String toString()
	{
		return this.numerator + "/" + this.denominator;
	}
	
	public Ratio getCanceled()
	{
		int gcd = getGcd(this.numerator, this.denominator);
		return new Ratio(this.numerator / gcd, this.denominator / gcd);
	}
	
	public int getFloored()
	{
		return (int) Math.floor((double)this.numerator / (double)this.denominator);
	}
	
	public int getCeiled()
	{
		return (int) Math.ceil((double)this.numerator / (double)this.denominator);
	}
	
	public int getRounded()
	{
		return (int) Math.round((double)this.numerator / (double)this.denominator);
	}
	
	private static int getGcd(int a, int b)
	{
		if (b == 0)
		{
			return a;
		}
		
		return getGcd(b, a % b);
	}
}