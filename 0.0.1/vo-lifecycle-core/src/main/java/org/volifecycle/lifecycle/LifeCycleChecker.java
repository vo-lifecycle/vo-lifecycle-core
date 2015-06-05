package org.volifecycle.lifecycle;

import java.util.List;
import java.util.Map;

/**
 * Checker interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public interface LifeCycleChecker<T> {
    /**
     * Return the result
     * 
     * @param valueObject
     * @return array with
     *         <ul>
     *         <li>item 0 :"true" if success or "false"</li>
     *         <li>item 1 : failed predicate (may be null)</li>
     *         </ul>
     */
    String[] getResult(T valueObject);

    /**
     * Return the result
     * 
     * @param valueObject
     * @param actionStorage
     * @return array with
     *         <ul>
     *         <li>item 0 :"true" if success or "false"</li>
     *         <li>item 1 : failed predicate (may be null)</li>
     *         </ul>
     */
    String[] getResult(T valueObject, Map<String, Object> actionStorage);

    /**
     * @return the id
     */
    String getId();

    /**
     * @return the description
     */
    String getDescription();

    /**
     * @return the targetState
     */
    String getTargetState();

    /**
     * @return the predicates
     */
    List<LifeCyclePredicate<T>> getPredicates();
}
