//package moredeli;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;


public class Moredeli {

public static void main (String[] args )throws IOException{
		
	String file_name = args[0];
	
	try{
		Map map = new Map(file_name);
		int[] start = { map.start_coor[0], map.start_coor[1],0} ;
		
		ArrayList<int[]> Q0 = new ArrayList<int[]>();
		ArrayList<int[]> Q1 = new ArrayList<int[]>();
		ArrayList<int[]> Q2 = new ArrayList<int[]>();
		ArrayList<int[]> Q3 = new ArrayList<int[]>();
		
		Q0.add(start);
		
		ArrayList<Integer> source= new ArrayList<Integer>();
		source.add(start[0]);
		source.add(start[1]);
		source.add(start[2]);
		Object[] arr = {start[0],start[1],'W',0};
		
		Hashtable<ArrayList<Integer>, Object[]> Prev = new Hashtable<ArrayList<Integer>, Object[]>();
		
		Prev.put(source,arr); // also has cost
		
		char[] moves = {'R','L','D','U'};
		dijkstra solution = new dijkstra(start, map);
		System.out.println(solution.find_path(Q0,Q1,Q2,Q3,Prev, map, 0, moves));
	}
	
	
	catch (IOException e) {
		System.out.println(e.toString());
	}
}
}
