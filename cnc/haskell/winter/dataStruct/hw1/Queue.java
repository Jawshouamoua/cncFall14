class Queue {

	int[] q ;
	int index = 0 ;
	int size = 100 ;

	public Queue() {
		this.q = new int[this.size] ;
	}

	public boolean isEmpty() {
		if(this.index==0)
			return true ;
		else
			return false ;
	}

	public void enqueue(int k) {
		this.q[index] = k ;
		if(this.index < this.size)
			this.index++ ;
	}	
	
	public int dequeue() {
		int temp = this.q[this.index] ;
		this.index-- ;
		return temp ;
	}

	public void print() {
		for(int i = 0; i <= this.index; i++) 
			System.out.print(this.q[i] + ",") ;

		System.out.println(" ") ;
	}

	public static void main(String[] args) {

		Queue q1 = new Queue() ;
		for(int i=0; i < 10; i++) {
			q1.enqueue(i) ;
			q1.print() ;
		}

		for(int i=0; i<10; i++) {
			q1.dequeue() ;
			q1.print() ;
		}

	}

}


