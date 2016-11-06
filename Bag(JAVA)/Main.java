
public class Main {
	
	public static void main(String [] args){
		
		Bag<Integer> saco = new SortedLinkedBag<>();
		saco.insert(5);
		saco.insert(4);
		saco.insert(8);
		saco.insert(9);
		saco.insert(10);
		saco.insert(8);
		
		System.out.println(saco.toString());
		System.out.println("size: " + saco.size());
		System.out.println("ocurrencias de 8: " + saco.getOccurrences(8));
		
		saco.delete(8);
		saco.delete(10);
		
		System.out.println(saco.toString());
		System.out.println("size: " + saco.size());
		System.out.println(saco.isElem(5));
		System.out.println("ocurrencias de 5: " + saco.getOccurrences(5));
		
	}

}
