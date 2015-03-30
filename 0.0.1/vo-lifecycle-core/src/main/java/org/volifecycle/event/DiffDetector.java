package org.volifecycle.event;

/**
 * Diff detector interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public interface DiffDetector<T> {
	/**
	 * Return true if there are differences between vo1 and vo2
	 * 
	 * @param vo1
	 *            value object 1
	 * @param vo2
	 *            value object 2
	 * @return true or false
	 */
	boolean compare(T vo1, T vo2);
}
