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
		return this.numerator + "/" + this.denominator;
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
}