public class Uml2Cdl
{
  static {
    System.loadLibrary("uml2cdl");
  }

    public static   native   int uml2cdl( String input_file_name, String output_file_name );

  public static void main( String args[] )
  {
      uml2cdl("input.xmi","output.xmi");
  }
}

