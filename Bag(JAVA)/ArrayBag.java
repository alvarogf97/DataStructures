/**
 * @author: Alvaro Garcia Fernandez
 *
 * Bag ADT implementation using parallel arrays.
 */


import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class ArrayBag<T extends Comparable<? super T>> implements Bag<T> {

	private final static int INITIAL_CAPACITY = 10;

	protected T[] elements; 	 // keep this array sorted
	protected int[] repetitions; // keep only those such that repetitions are > 0
	protected int nextFree;		 // last element is stored in cell nextFree-1
	protected int size;			 // total number of elements (sum of all counters)


	@SuppressWarnings("unchecked")
	public ArrayBag() {
		elements = (T[]) new Object[INITIAL_CAPACITY];
		repetitions = new int [INITIAL_CAPACITY];
		nextFree = 0;
		size = 0;
	}


	private void ensureCapacity() {
		if (nextFree == elements.length) {
			elements = Arrays.copyOf(elements, 2 * elements.length);
			repetitions = Arrays.copyOf(repetitions, 2 * repetitions.length);
		}
	}

	public boolean isEmpty(){
		return size == 0; 
	}

	public int size(){
		 return size; 
	}

	public void insert(T item) {
		ensureCapacity();
		int i = isElem(item);
		if(i>-1){
			repetitions[i]++;
		}else{
			int PosAOcupar = 0;
			int j = 0;
			boolean parar = false;
			while(j<nextFree && parar == false){
				if(elements[j].compareTo(item) > 0){
					parar = true;
					PosAOcupar = j;
				}
			}
			for(int m = nextFree; m>PosAOcupar; m++){
				elements[m] = elements[m-1];
				repetitions[m] = repetitions[m-1];
			}
			elements[PosAOcupar] = item;
			repetitions[PosAOcupar] = 1;
		}
		size++;
		nextFree++;
	}

	public int isElem(T x) {
		boolean encontrado = false;
		int i = 0;
		while(i<nextFree && encontrado == false){
			if(elements[i] == x){
				encontrado = true;
			}else if(elements[i].compareTo(x) > 0){
				encontrado = true;
				i = -1;
			}
			i++;
		}
		return i--;
	}

	public int getOccurrences(T x) {
		int res = 0;
		int i = isElem(x);
		if(i>-1){
			res = repetitions[i];
		}
		return res;
	}

	public void delete(T item) {
		int i = isElem(item);
		if(i>-1){
			if(repetitions[i] == 1){
				for(int m = i; m<nextFree; m++){
					elements[m]=elements[m+1];
					repetitions[m]=repetitions[m+1];
				}
				size--;
				nextFree--;
			}else{
				repetitions[i]--;
				size--;
			}
		}
	}
	
	//Iterator
	private class BagIterator implements Iterator<T> {
		// toDo
		int pos;
		int acumulador;
		
		BagIterator() {
			pos = 0;
			acumulador = 1;
		}

		public boolean hasNext() {
			return elements[pos]!=null;
		}

		public T next() {
			if(!hasNext()){
				throw new NoSuchElementException();
			}
			T elem = elements[pos];
			if(repetitions[pos]==acumulador){
				acumulador = 1;
				pos++;
			}else{
				acumulador++;
			}
			return elem;
		}
	}

	public Iterator<T> iterator() {
		return new BagIterator();
	}

	public String toString() {
		String className = getClass().getSimpleName();
		String text = className+"(";
		for (int i = 0; i < nextFree; i++) {
			text += "(" + elements[i] + ", " + repetitions[i] + ")" + (i<nextFree-1 ? "," : "");
		}
		return text + ")";
	}

}
