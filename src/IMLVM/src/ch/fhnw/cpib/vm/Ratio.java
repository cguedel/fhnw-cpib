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