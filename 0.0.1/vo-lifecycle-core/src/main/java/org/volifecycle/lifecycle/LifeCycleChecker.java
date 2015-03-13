package org.volifecycle.lifecycle;

/**
 * Checker interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            objet métier
 */
public interface LifeCycleChecker<T> {
	/**
	 * Return the result
	 * 
	 * @param valueObject
	 * @return "true" if success or "false"
	 */
	String getResult(T valueObject);

	/**
	 * @return the id
	 */
	String getId();

	/**
	 * @return the description
	 */
	String getDescription();
}
