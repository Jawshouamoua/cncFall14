import java.util.Comparator ;

public class BoardComparator implements Comparator<Board> {

	public int compare(Board b,Board nb) {

		if((b.hamming()+b.moves()) < (nb.hamming()+nb.moves())) 
			return -1 ;

		if((b.hamming()+b.moves()) > (nb.hamming()+nb.moves())) 
			return 1 ;

		return 0 ;

	}
}
