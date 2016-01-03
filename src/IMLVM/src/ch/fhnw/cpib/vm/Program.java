package ch.fhnw.cpib.vm;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
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

		if (args.length != 1) {
			System.err.println("Expected path as argument");
			System.exit(1);
		}

		File codeFile = new File(args[0]);
		if (!codeFile.exists()) {
			System.err.println("File does not exist: " + args[0]);
			System.exit(1);
		}

		FileInputStream fStream = new FileInputStream(args[0]);
		BufferedReader br = new BufferedReader(new InputStreamReader(fStream));

		String strLine;
		List<String> instr = new ArrayList<String>();

		while ((strLine = br.readLine()) != null) {
			instr.add(strLine);
		}

		br.close();
		fStream.close();

		CodeArray code = new CodeArray(instr.size());
		for (int i = 0; i < instr.size(); i++) {
			IInstr c = getInstrFromString(instr.get(i));
			System.out.println("[" + i + "] = " + c);
			code.put(i, c);
		}

		new VirtualMachine(code, 1024);
	}

	private static IInstr getInstrFromString(String instr) {
		int instrNameLength = instr.indexOf('(');
		String instrName = instr.substring(0,
				instrNameLength > 0 ? instrNameLength : instr.length());

		instr = instr.substring(0, instr.length() - 1);
		String arg = instrNameLength > 0 ? instr.substring(instrNameLength + 1)
				: "";

		switch (instrName) {
		case "AddInt":
			return new IInstructions.AddInt();
		case "AddRatio":
			return new IInstructions.AddRatio();
		case "AllocBlock":
			return new IInstructions.AllocBlock(Integer.parseInt(arg));
		case "AllocStack":
			return new IInstructions.AllocStack(Integer.parseInt(arg));
		case "Call":
			return new IInstructions.Call(Integer.parseInt(arg));
		case "CeilRatio":
			return new IInstructions.CeilRatio();
		case "CondJump":
			return new IInstructions.CondJump(Integer.parseInt(arg));
		case "DenomRatio":
			return new IInstructions.DenomRatio();
		case "Deref":
			return new IInstructions.Deref();
		case "DivTruncInt":
			return new IInstructions.DivTruncInt();
		case "DivTruncRatio":
			return new IInstructions.DivTruncRatio();
		case "Dup":
			return new IInstructions.Dup();
		case "EqInt":
			return new IInstructions.EqInt();
		case "EqRatio":
			return new IInstructions.EqRatio();
		case "FloorRatio":
			return new IInstructions.FloorRatio();
		case "GeInt":
			return new IInstructions.GeInt();
		case "GeRatio":
			return new IInstructions.GeRatio();
		case "GtInt":
			return new IInstructions.GtInt();
		case "GtRatio":
			return new IInstructions.GtRatio();
		case "InputBool":
			return new IInstructions.InputBool(arg);
		case "InputInt":
			return new IInstructions.InputInt(arg);
		case "InputRatio":
			return new IInstructions.InputRatio(arg);
		case "LeInt":
			return new IInstructions.LeInt();
		case "LeRatio":
			return new IInstructions.LeRatio();
		case "LoadAddrRel":
			return new IInstructions.LoadAddrRel(Integer.parseInt(arg));
		case "LoadImInt":
			return new IInstructions.LoadImInt(Integer.parseInt(arg));
		case "LoadImRatio":
			return new IInstructions.LoadImRatio(Ratio.fromString(arg));
		case "LtInt":
			return new IInstructions.LtInt();
		case "LtRatio":
			return new IInstructions.LtRatio();
		case "ModTruncInt":
			return new IInstructions.ModTruncInt();
		case "MultInt":
			return new IInstructions.MultInt();
		case "MultRatio":
			return new IInstructions.MultRatio();
		case "NegInt":
			return new IInstructions.NegInt();
		case "NegRatio":
			return new IInstructions.NegRatio();
		case "NeInt":
			return new IInstructions.NeInt();
		case "NeRatio":
			return new IInstructions.NeRatio();
		case "NumRatio":
			return new IInstructions.NumRatio();
		case "OutputBool":
			return new IInstructions.OutputBool(arg);
		case "OutputInt":
			return new IInstructions.OutputInt(arg);
		case "OutputRatio":
			return new IInstructions.OutputRatio(arg);
		case "Return":
			return new IInstructions.Return(Integer.parseInt(arg));
		case "RoundRatio":
			return new IInstructions.RoundRatio();
		case "Stop":
			return new IInstructions.Stop();
		case "Store":
			return new IInstructions.Store();
		case "SubInt":
			return new IInstructions.SubInt();
		case "SubRatio":
			return new IInstructions.SubRatio();
		case "UncondJump":
			return new IInstructions.UncondJump(Integer.parseInt(arg));
		}

		throw new UnsupportedOperationException();
	}
}
