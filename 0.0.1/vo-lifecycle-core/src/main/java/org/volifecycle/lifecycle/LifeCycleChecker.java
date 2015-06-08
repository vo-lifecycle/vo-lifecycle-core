package org.volifecycle.lifecycle;

import java.util.List;
import java.util.Map;

/**
 * Checker interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public interface LifeCycleChecker<T> {
    /**
     * Return the result.
     * 
     * @param valueObject
     * @param failedPredicate
     * @return "true" if success or "false"
     */
    String getResult(T valueObject, List<String> failedPredicate);

    /**
     * Return the result.
     * 
     * @param valueObject
     * @param failedPredicate
     * @param actionStorage
     * @return "true" if success or "false"
     */
    String getResult(T valueObject, List<String> failedPredicate, Map<String, Object> actionStorage);

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

    /**
     * @return the additionnalInformations
     */
    Map<String, String> getAdditionnalInformations();

    /**
     * @return the stopIfFailed
     */
    Boolean getStopIfFailed();

    /**
     * @param stopIfFailed
     *            the stopIfFailed to set
     */
    void setStopIfFailed(Boolean stopIfFailed);
}
