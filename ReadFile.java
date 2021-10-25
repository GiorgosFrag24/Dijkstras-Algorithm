//package moredeli;

//package textfiles;

import java.io.IOException;
import java.io.FileReader;
import java.io.BufferedReader;


public class ReadFile {
	
	protected String path;
	public ReadFile(String file_path) {
		path = file_path;
	}
	public int[] getDim() throws IOException{
		FileReader fr = new FileReader(path);
		BufferedReader textReader = new BufferedReader (fr);
		
		int N = readLines();
		int M = readColumns();
		int [] dim ={N,M}; 
		textReader.close();
		return  dim ; 
	}
	
	int readLines() throws IOException {
		FileReader file_to_read = new FileReader(path);
		BufferedReader bf = new BufferedReader(file_to_read);
		
		int numberOfLines = 0;
		
		while ((bf.readLine()) != null) {
			numberOfLines ++;
		}
		bf.close(); 
		
		return numberOfLines;
	}
	int readColumns() throws IOException {
		FileReader file_to_read = new FileReader(path);
		BufferedReader bf = new BufferedReader(file_to_read);
		
		int numberOfColumns = 0;
		
		while (((char) bf.read()) != '\n'){// && ((char) bf.read()) != '\r' && ((char) bf.read()) != '\t' ) {
			numberOfColumns ++;
		}
		bf.close(); 
		
		return numberOfColumns;
	}

	
	
	} 

