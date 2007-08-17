import java.net.*;
import java.io.*;

public class Uml2CdlClient{
    
    public static void main(String[] args) throws Exception{
	if (args.length != 1){
	    System.out.println("Usage: java Uml2CdlClient <XMI>");
	    System.exit(0);
	}

	//open a socket and corresponding input and output streams
	Socket s = new Socket("lennox",443);

	BufferedOutputStream buf_sock_out 
	    = new BufferedOutputStream(s.getOutputStream());
	BufferedInputStream buf_sock_ins 
	    = new BufferedInputStream(s.getInputStream());

	//read a file and write it on the socket
	BufferedInputStream buf_infile 
	    = new BufferedInputStream(new FileInputStream(args[1]));
	
	byte[] b = new byte[1000];
	int i ;
	while((i = buf_infile.read(b,0,1000)) != -1){
	    buf_sock_out.write(b,0,i);
	}
	buf_sock_out.flush();

	//if we close the stream, as it will also close the socket.
	//therefore we just "close" the output half of the socket
	s.shutdownOutput();
	
	//read result from socket and print on stdout
	try { 
	    while(!s.isClosed() && 
		  (i = buf_sock_ins.read(b,0,1000)) != -1){
		System.out.write(b,0,i);
		System.out.flush();
	    }
	    //we catch the socketexception, because the server just shuts down 
	    //the connection after he has sent everything...
	} catch (SocketException msg){} 

	buf_sock_ins.close();
    }
}
