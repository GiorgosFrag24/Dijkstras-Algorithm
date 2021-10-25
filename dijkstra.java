//package moredeli;

import java.util.Hashtable;
import java.util.ArrayList;

public class dijkstra {
	
	
	public dijkstra(int[] source,Map map){
	}
	public static String compute_path(ArrayList<Integer> current,Hashtable<ArrayList<Integer>,Object[]> prev){
		Object[] previous = prev.get(current); 
		if(previous==null){
			return "xoxoxo";
		}
		Object x =  previous[0];
		Object y =  previous[1];
		Object cost = previous[3];
		if((char)previous[2] == 'W'){
			return "";
		}
		else{
			ArrayList<Integer> other = new ArrayList<Integer>();
			other.add((Integer) x);
			other.add((Integer) y);
			other.add((Integer)cost);
		return compute_path(other, prev)+previous[2];
		}
		
	}
	public String find_path (ArrayList<int[]> Q0,ArrayList<int[]> Q1,ArrayList<int[]> Q2,ArrayList<int[]> Q3,Hashtable<ArrayList<Integer>, Object[]> prev,Map map,int tempCost,char[] moves){		
		if (Q0.size()!= 0) {
				int[] current = (Q0.get(0));
				Q0.remove(0);
				int x = current[0];
				int y = current[1];
				int cost = current[2];
				ArrayList<Integer> node = new ArrayList<Integer>();
				node.add(x);
				node.add(y);
				node.add(cost);
					if(map.map_array[x][y] == 'E'){
					//System.out.print("I got here !!!\n");
					int finalcost = cost;
					String result = Integer.toString(finalcost) + " " + compute_path(node,prev);
					return result;
					}
				else   {
					for (char i : moves){
						if (i=='R') {
							if(map.in_bounds(x, y+1)){
								ArrayList<Integer> next = new ArrayList<Integer>();
								next.add((Integer)x);
								next.add((Integer)y+1);
								next.add(cost+1);
								if(prev.containsKey(next)!=true){
									Object[] arr = {x,y,'R',tempCost};
									int[] malakia = {x,y+1,tempCost+1};
									prev.put(next,arr);
									Q1.add(malakia);
								}
							}
						}
						else if (i=='L'){
							if(map.in_bounds(x, y-1)){
								ArrayList<Integer> next = new ArrayList<Integer>();
								next.add(x);
								next.add(y-1);
								next.add(cost+2);
								if(prev.containsKey(next)!=true){
									Object[] arr = {x,y,'L',tempCost};
									prev.put(next,arr);
									int[] malakia = {x,y-1,tempCost+2};
									Q2.add(malakia);
								}
							}
						}				
						else if (i=='D'){
							if(map.in_bounds(x+1, y)){
								ArrayList<Integer> next = new ArrayList<Integer>();
								next.add(x+1);
								next.add(y);
								next.add(cost+1);
								if(prev.containsKey(next)!=true){
									Object[] arr = {x,y,'D',tempCost};
									prev.put(next,arr);
									int[] malakia = {x+1,y,tempCost+1};
									Q1.add(malakia);
								}
							}
						}
						else if (i=='U'){
							if(map.in_bounds(x-1, y)){
								ArrayList<Integer> next = new ArrayList<Integer>();
								next.add(x-1);
								next.add(y);
								next.add(cost+3);
								if(prev.containsKey(next)!=true){
									Object[] arr = {x,y,'U',tempCost};
									prev.put(next,arr);
									int[] malakia = {x-1,y,tempCost+3};
									Q3.add(malakia);
								}
							}
						}
					}
				}
				return  find_path (Q0,Q1,Q2,Q3, prev, map, tempCost, moves);
				
		}
		else {
				 return find_path (Q1,Q2,Q3,new ArrayList<int[]>(), prev, map, tempCost+1, moves);
				 
		}
	}
}



		
	
	
	
	
	
