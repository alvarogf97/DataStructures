/**
 * @author <<write here your name>> 
 * 
 * Maxiphobic Heaps
 * Data Structures, Grado en Informática. UMA.
 *
 */
 
package dataStructures.heap;


/**
 * Heap implemented using maxiphobic heap-ordered binary trees.
 * @param <T> Type of elements in heap.
 */
public class MaxiphobicHeap<T extends Comparable<? super T>> implements	Heap<T> {

	protected static class Tree<E> {
		E elem;
		int weight;
		Tree<E> left;
		Tree<E> right;
	}

	private static int weight(Tree<?> heap) {
		return heap == null ? 0 : heap.weight;
	}
	
	private static int maximo(int a,int b, int c){
		if(a>b && a>c){
			return a;
		}else if(b>a && b> c){
			return b;
		}else{
			return c;
		}
	}
	
	private static <T> Tree<T> Construir(T x, Tree<T> h1, Tree<T> h2){
		int w1 = weight(h1);
		int w2 = weight(h2);
		
		Tree<T> tree = new Tree<T>();
		tree.elem = x;
		tree.weight = w1+w2+1;
		tree.right = h2;
		tree.left = h1;
		return tree;
	}

	private static <T extends Comparable<? super T>> Tree<T> merge(Tree<T> h1,	Tree<T> h2) {
		if(h1 == null){
			return h2;
		}else if(h2 == null){
			return h1;
		}
		
		T x = h1.elem;
		T x2 = h2.elem;
		if(x.compareTo(x2)<=0){
			if(maximo(weight(h1.left),weight(h1.right),weight(h2)) == weight(h1.left)){
				return Construir(x, merge(h1.right,h2), h1.left);
			}else if(maximo(weight(h1.left),weight(h1.right),weight(h2)) == weight(h1.right)){
				return Construir(x, merge(h1.left,h2), h1.right);
			}else{
				return Construir(x, merge(h1.right,h1.left), h2);
			}
		}else{
			if(maximo(weight(h2.left),weight(h2.right),weight(h1)) == weight(h2.left)){
				return Construir(x, merge(h2.right,h1), h2.left);
			}else if(maximo(weight(h2.left),weight(h2.right),weight(h1))  == weight(h2.right)){
				return Construir(x, merge(h2.left,h1), h2.right);
			}else{
				return Construir(x, merge(h2.right,h2.left), h1);
			}
		}
	}

	protected Tree<T> root;

	/**
	 * Constructor: creates an empty Maxiphobic Heap. 
	 * <p>Time complexity: ???
	 */
	public MaxiphobicHeap() {
		root = null;
	}

	/** 
	 * <p>Time complexity: ???
	 */
	public boolean isEmpty() {
		return root == null;
	}

	/** 
	 * <p>Time complexity: ???
	 * @throws <code>EmptyHeapException</code> if heap stores no element.
	 */
	public T minElem() {
		if(isEmpty()){
			throw new EmptyHeapException("minElem on empty maxiphobicHeap");
		}else{
			return root.elem;
		}
	}

	/** 
	 * <p>Time complexity: ???
	 * @throws <code>EmptyHeapException</code> if heap stores no element.
	 */
	public void delMin() {
		if(isEmpty()){
			throw new EmptyHeapException("DelMin on empty maxiphobicHeap");
		}else{
			root = merge(root.left,root.right);
		}
	}

	/** 
	 * <p>Time complexity: ???
	 */
	public void insert(T value) {
		Tree<T> tree = new Tree<>();
		tree.elem = value;
		tree.weight = 1;
		tree.left = null;
		tree.right = null;
		root = merge(root,tree);
	}

	private static String toStringRec(Tree<?> tree) {
		return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
				+ tree.elem + "," + toStringRec(tree.right) + ">";
	}
	
	/** 
	 * Returns representation of heap as a String.
	 */
	public String toString() {
		String className = getClass().getSimpleName();

		return className+"("+toStringRec(this.root)+")";
	}

	
	public int size() {
		return isEmpty() ? 0 : root.weight;
	}	
	
}