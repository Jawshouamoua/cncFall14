import java.util.* ;
import java.lang.* ;

public class Solver {

	static Comparator<Board> comp = new BoardComparator() ;
	PriorityQueue<Board> pQueue = new PriorityQueue<Board>(comp) ;
	Iterable<Board> holder ;
	Vector<Board> soln = new Vector<Board>() ;
	Board prevB, node ;
	int moves = 0;


	//find a solution to the initial board
	public Solver(Board initial) {

		pQueue.add(initial) ;	
		prevB = initial ;
		node = pQueue.poll() ;
	//	System.out.println("Start: \n" + initial.print()) ;
		while(!node.isGoal()) {


			if(node.isGoal())
				System.out.println("Goal") ;

			//get neighbors and add to pQueue
			holder = node.neighbors() ;
			for(Board b : holder) {
				if(b.equals(prevB))
					continue ;

	//			System.out.println("Child: \n" + b.print()) ;
				pQueue.add(b) ;
			}

			soln.add(node) ;
			prevB = node ;
			node = pQueue.poll() ;
	//		System.out.println("new search node: \n" + node.print()) ;

		}

		soln.add(node) ;
		moves = node.moves() ;
	}

	//min number of moves to solve initial board
	public int moves() {
	
		return moves ;
	}

	//sequence of boards in a shortest solution
	public Iterable<Board> solution() {
		return soln ;
	}


	public static void main(String[] args) {
		
		//create initial board
		//int[][] b1 = { {3,1,2}, {4,0,5}, {6,7,8} } ;
		int[][] b1 = {{7,3,0},{1,4,8},{5,6,2}} ;
		//int[][] b1 = {{1,2,8},{3,4,0},{6,5,7}} ;
		int[][] b2 = new int[3][3] ;

		List<Integer> seed = new ArrayList<Integer>() ;
		for(int i=0;i<9;i++)
			seed.add(i) ;

		Collections.shuffle(seed) ;
		int cnt = 0 ;
		for(int i=0;i<3;i++) {
			for(int j=0;j<3;j++) {
				b2[i][j] = seed.get(cnt) ;
				cnt++ ;
			}
		}

		Board initial = new Board(b2,0) ;

		//check if puzzle is unsolvable
		//if so, solve it and output soln

		if(initial.isSolvable()) {
			Solver solver = new Solver(initial) ;
			System.out.println("min number of moves = " + solver.moves());
			for(Board board : solver.solution())
				System.out.println(board.print()) ;
		}

		//if not, report unsolvable
		else {
			System.out.println("Unsolvable puzzle") ;

		}

	}

}	
