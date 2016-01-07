package ch.fhnw.cpib.vm;

public class RatioTest {

	public static void main(String[] args) {
		
		
		Ratio r = new Ratio(400, 4);
		
		Ratio gr = r.getCanceled();
		
		System.out.println(gr.toString());
		
		System.out.println(gr.isPeriodic(r));
		
		double d = gr.getRatioFormat();
		
		int i = gr.getInteger();
		
		System.out.println(d);
		System.out.println(i);
		

	}
	
	
}
