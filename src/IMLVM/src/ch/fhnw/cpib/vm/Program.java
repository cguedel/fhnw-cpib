package ch.fhnw.cpib.vm;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import ch.fhnw.cpib.vm.ICodeArray.CodeTooSmallError;
import ch.fhnw.cpib.vm.IInstructions.IInstr;
import ch.fhnw.cpib.vm.IVirtualMachine.ExecutionError;

public class Program {

	public static void main(String[] args) throws CodeTooSmallError,
			ExecutionError, IOException {

		args = new String[] { "C:\\Users\\cg\\Documents\\Documents\\FHNW\\cpib\\code\\build\\code.txt" };
		
		if (args.length != 1)
		{
			System.err.println("Expected path as argument");
			System.exit(1);
		}
		
		File codeFile = new File(args[0]);
		if (!codeFile.exists())
		{
			System.err.println("File does not exist: " + args[0]);
			System.exit(1);
		}
		
		FileInputStream fStream = new FileInputStream(args[0]);
		BufferedReader br = new BufferedReader(new InputStreamReader(fStream));
		
		String strLine;
		List<String> instr = new ArrayList<String>();
		
		while ((strLine = br.readLine()) != null)
		{
			instr.add(strLine);
		}
		
		br.close();
		fStream.close();
		
		CodeArray code = new CodeArray(instr.size());
		for (int i = 0; i < instr.size(); i++)
		{
			IInstr c = getInstrFromString(instr.get(i));
			System.out.println("[" + i + "] = " + c);
			code.put(i, c);
		}
		
		new VirtualMachine(code, 1024);
	}

	private static IInstr getInstrFromString(String instr)
	{
		int instrNameLength = instr.indexOf('(');
		String instrName = instr.substring(0, instrNameLength > 0 ? instrNameLength : instr.length());
		
		instr = instr.substring(0, instr.length() - 1);
		String arg = instrNameLength > 0 ? instr.substring(instrNameLength + 1) : "";
		
		switch (instrName)
		{
			case "LoadImRatio":
				return new IInstructions.LoadImRatio(Ratio.fromString(arg));
			case "DenomRatio":
				return new IInstructions.DenomRatio();
			case "AddInt":
				return new IInstructions.AddInt();
			case "LoadImInt":
				return new IInstructions.LoadImInt(Integer.parseInt(arg));
			case "GtInt":
				return new IInstructions.GtInt();
			case "OutputBool":
				return new IInstructions.OutputBool(arg);
			case "Stop":
				return new IInstructions.Stop();
		}
		
		throw new UnsupportedOperationException();
	}
}
