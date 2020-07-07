import java.io.IOException;

public class c3{
   public static int main() throws IOException{
      int a;

      a=lib.Runtime.readInt();
      lib.Runtime.printInt(a);
  
      String b;
      b=lib.Runtime.readString();
      lib.Runtime.printString(b);

      return a;
   }
   
   public static void main(String[] args) throws IOException{
      System.exit(c3.main());
   }
}
