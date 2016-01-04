package ch.fhnw.cpib.vm;

import ch.fhnw.cpib.vm.ICodeArray.CodeTooSmallError;
import ch.fhnw.cpib.vm.IVirtualMachine.ExecutionError;

public class Tests {

	public static void main(String[] args) throws CodeTooSmallError, ExecutionError {
		CodeArray c = new CodeArray(15);
		
		c.put(0, new IInstructions.AllocBlock(1)); // platz für var
		c.put(1, new IInstructions.LoadImInt(0)); // addr
		c.put(2, new IInstructions.LoadImInt(34)); // val
		c.put(3, new IInstructions.Store());
		
		c.put(4, new IInstructions.AllocBlock(1));
		c.put(5, new IInstructions.LoadImInt(1));
		c.put(6, new IInstructions.LoadImInt(42));
		c.put(7, new IInstructions.Store());
		
		c.put(8, new IInstructions.LoadImInt(1)); // addr
		c.put(9, new IInstructions.Deref());
		c.put(10, new IInstructions.OutputInt(""));
		
		c.put(11, new IInstructions.LoadImInt(0)); // addr
		c.put(12, new IInstructions.Deref());
		c.put(13, new IInstructions.OutputInt(""));
		
		c.put(14, new IInstructions.Stop());
		
		new VirtualMachine(c, 1024);
	}

}
