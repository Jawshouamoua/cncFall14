import java.util.Comparator ;

public class BoardComparator implements Comparator<Board> {

	public int compare(Board b,Board nb) {

		if(b.hamming() < nb.hamming()) 
			return -1 ;

		if(b.hamming() > nb.hamming()) 
			return 1 ;

		return 0 ;

	}
}
