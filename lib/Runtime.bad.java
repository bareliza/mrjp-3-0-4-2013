package lib;

import java.io.Console;

public class Runtime {
	public static String readString() {
	        Console console = System.console();
		return console.readLine();
	}
	
	public static int readInt() {
		String line=readString();
		
		return Integer.parseInt(line);
	}
	
	public static void printString(String s) {
		System.out.println(s);
	}

	public static void printInt(int i) {
		System.out.println(i);
	}

	public static void error(){
		System.exit(1);
	}
}
