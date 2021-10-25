//package moredeli;

import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;
//import textfiles.ReadFile;

public class Map extends ReadFile {

	public Map(String file_path) throws IOException {
		super(file_path);
	}
	
	public int dim[] = getDim();
	public char [][] makeMap(int [] dim,String file_path) throws IOException {
		char[][] myArray = new char[dim[0]][];
		FileReader fr = new FileReader(file_path);
		Scanner in = new Scanner(fr) ;
		for (int row = 0; in.hasNextLine() && row < dim[0]; row++) {
		    myArray[row] = in.nextLine().toCharArray();
		}
		in.close();
		return myArray;
	}
	
	
	public int[] find_element(char element,char[][] map ) {
		int [] coor = {0,0};
		for (int i = 0 ;i<dim[0];i++){
			for (int j =0;j<dim[1];j++){
				if (map[i][j] == element){
					coor[0] = i;
					coor[1] = j;
					break;
				}
			}
		}
		return coor;
	}
	public boolean in_bounds (int i,int j) {
		if ((i>=0 && i<dim[0] && j>=0 && j<dim[1])) {
			if(map_array[i][j]!= 'X' && map_array[i][j]!= '\n' && map_array[i][j]!= '\r' && map_array[i][j]!= '\t' ){
				return true;
			}
		}
		return false;
	}


	public  char [][] map_array = makeMap(dim,path);
	public  int [] start_coor = find_element('S',map_array);
	public int [] end_coor = find_element('E',map_array);
	
	
}
