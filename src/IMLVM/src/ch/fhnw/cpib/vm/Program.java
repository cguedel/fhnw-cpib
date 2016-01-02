package ch.fhnw.cpib.vm;

import ch.fhnw.cpib.vm.ICodeArray.CodeTooSmallError;
import ch.fhnw.cpib.vm.IVirtualMachine.ExecutionError;

public class Program {

	public static void main(String[] args) throws CodeTooSmallError,
			ExecutionError {
		// TODO Auto-generated method stub
		CodeArray code = new CodeArray(5);
		code.put(0, new IInstructions.LoadImInt(50));
		code.put(1, new IInstructions.LoadImInt(-51));
		code.put(2, new IInstructions.AddInt());
		code.put(3, new IInstructions.OutputInt("x"));
		code.put(4, new IInstructions.Stop());

		new VirtualMachine(code, 20);

		CodeArray code2 = new CodeArray(3);
		code2.put(0, new IInstructions.LoadImInt(1));
		code2.put(1, new IInstructions.OutputBool(""));
		code2.put(2, new IInstructions.Stop());

		new VirtualMachine(code2, 20);

		CodeArray code3 = new CodeArray(3);
		code3.put(0, new IInstructions.LoadImRatio(new Ratio(34, 34)));
		code3.put(1, new IInstructions.OutputRatio(""));
		code3.put(2, new IInstructions.Stop());	
		
		new VirtualMachine(code3, 20);
		
		CodeArray code4 = new CodeArray(4);
		code4.put(0, new IInstructions.LoadImRatio(new Ratio(3, 4)));
		code4.put(1, new IInstructions.CeilRatio());
		code4.put(2, new IInstructions.OutputInt(""));
		code4.put(3, new IInstructions.Stop());
		
		new VirtualMachine(code4, 20);
	}

}
