import java.util.Iterator;
import java.util.NoSuchElementException;

public class SortedLinkedBag<T extends Comparable<? super T>> implements Bag<T>{
	
	// implementamos la clase Node como en haskell
	static private class Node<E>{
		E elem; // elemento
		private int ocurr; // ocurrencias
		Node<E> next; // siguiente
		
		// constructor
		Node(E x, int ocurrencias,Node<E> node){
			elem = x;
			setOcurr(ocurrencias);
			next = node;
		}
		
		//getter and setter para las ocurrencias
		public int getOcurr() {
			return ocurr;
		}

		public void setOcurr(int ocurr) {
			this.ocurr = ocurr;
		}
	}
	
	// variables de instancia
	
	private Node<T> first;
	private int size;
	
	public boolean isEmpty(){
		return size == 0;
	}
	
	public int size(){
		return size;
	}
	
	public void insert(T item){
		Node<T> previous = null;
		Node<T> current = first;
		
		// comprobamos si esta o no en la bolsa
		while (current != null && current.elem.compareTo(item) < 0){
			previous = current;
			current = current.next;
		}
		
		boolean found = (current != null) && current.elem.equals(item);
		//añadimos donde corresponda
		  if(!found) {
	            if(previous == null){
	            	// lo insertamos en la primera posicion
	            	first = new Node<>(item,1, first);
	            }else{
	            	previous.next = new Node<>(item,1, current);
	            }
	      }else{
	    	  current.setOcurr(current.getOcurr() + 1); //añadimos una ocurrencia mas
	      }
		  size++;
		
	}
	
	public boolean isElem(T item){
		//usamos el mismo procedimiento que en insert
		Node<T> current = first;
		
		while(current != null && current.elem.compareTo(item) < 0){
			current = current.next;
		}
		
		return(current != null && current.elem.equals(item));
	}
	
	public int getOccurrences(T item){
		int res = 0;
		Node<T> current = first;
		
		while(current != null && current.elem.compareTo(item) < 0){
				current = current.next;
		}
		
		if(current != null && current.elem.equals(item)){
			res = current.getOcurr();
		}
		return res;
	}
	
	public void delete(T item){
		Node<T> previous = null;
		Node<T> current = first;
		
		while(current != null && current.elem.compareTo(item)<0){
			previous = current;
			current = current.next;
		}
		
		boolean found = (current != null) && current.elem.equals(item);
		// si se encuentra eliminamos una ocurrencia si es 0 la quitamos
		if(found){
			if(previous == null){
				if(current.ocurr-1 == 0){
					first = current.next;
				}else{
					current.setOcurr(current.getOcurr()-1);
				}
			}else{
				if(current.ocurr-1 == 0){
					previous.next = current.next;
				}else{
					current.setOcurr(current.getOcurr()-1);
				}
			}
			size--;
		}
		
	}
	
	// rpresentacion de una bolsa
	public String toString() {
        String className = "SortedLinkedBag";
        StringBuilder text = new StringBuilder();
        text.append(className+"(");
        for (Node<T> p = first; p != null; p = p.next) {
        	for(int i = 0; i<p.ocurr; i++){
        		text.append(p.elem + ",");
        	}
        	text.deleteCharAt(text.length()-1);
            text.append(p.next != null ? "," : "");
        }
        return text.toString() + ")";
    }

	//para implementar el iterator creamos una clase
	public Iterator<T> iterator() {
		return new LinkedSetIterator();
	}
	
	
	private class LinkedSetIterator implements Iterator<T>{
		
		Node<T> current;
		
		public LinkedSetIterator(){
			current = first;
		}
		
		public boolean hasNext() {
			return current != null;
		}

		public T next() {
			if(!hasNext()){
				throw new NoSuchElementException();
			}
			
			T x = current.elem;
			current = current.next;
			return x;
		}
		
	}
	
	
	
}
