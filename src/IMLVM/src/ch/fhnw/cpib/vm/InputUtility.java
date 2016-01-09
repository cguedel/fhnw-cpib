// Virtual Machine Java 2015, V01
// Edgar F.A. Lederer, FHNW and Uni Basel, 2015

package ch.fhnw.cpib.vm;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import ch.fhnw.cpib.vm.IVirtualMachine.ExecutionError;

class InputUtility {

	private static BufferedReader reader = new BufferedReader(
			new InputStreamReader(System.in));

	public static boolean readBool() throws ExecutionError {
		String s;
		try {
			s = reader.readLine();
		} catch (IOException e) {
			throw new ExecutionError("Input failed.");
		}
		if (s.equals("false")) {
			return false;
		} else if (s.equals("true")) {
			return true;
		} else {
			throw new ExecutionError("Not a boolean.");
		}
	}

	public static int readInt() throws ExecutionError {
		String s;
		try {
			s = reader.readLine();
		} catch (IOException e) {
			throw new ExecutionError("Input failed.");
		}
		try {
			return Integer.parseInt(s);
		} catch (NumberFormatException e) {
			throw new ExecutionError("Not an integer.");
		}
	}

	public static Ratio readRatio() throws ExecutionError {
		String s;
		try {
			s = reader.readLine();
		} catch (IOException e) {
			throw new ExecutionError("Input failed.");
		}

		int separatorPos = s.indexOf('/');
		if (separatorPos > 0) {
			try {
				return new Ratio(
						Integer.parseInt(s.substring(0, separatorPos)),
						Integer.parseInt(s.substring(separatorPos + 1)));
			} catch (NumberFormatException e) {
				throw new ExecutionError(
						"Numerator or denominator not an integer.");
			}
		} else {
			try {
				return new Ratio(Integer.parseInt(s), 1);
			} catch (NumberFormatException e) {
				throw new ExecutionError("Invalid format");
			}
		}
	}
}
