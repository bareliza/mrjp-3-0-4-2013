package lib;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Runtime {
	static InputStreamReader converter;
	static BufferedReader in;
	
	public static String readString() throws IOException {
		if(converter==null){
			converter = new InputStreamReader(System.in);
			in = new BufferedReader(converter);
		}
		
		return in.readLine();
	}
	
	public static int readInt() throws IOException {
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
