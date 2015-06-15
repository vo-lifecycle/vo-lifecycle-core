package org.volifecycle.lifecycle.extra.jsr303;

import java.util.Map;
import java.util.Set;

import javax.validation.ConstraintViolation;

import org.volifecycle.lifecycle.LifeCycleAction;

/**
 * JSR303 support interface.
 * 
 * @author Idriss Neumann <idriss.neumann@capgemini.com>
 *
 * @param <T>
 */
public interface LifeCycleActionJSR303<T> extends LifeCycleAction<T> {
    /**
     * Return the result.
     * 
     * @param valueObject
     * @param actionStorage
     * @param constraintViolations
     * @return "true" if success or "false"
     */
    String getResult(T valueObject, Map<String, Object> actionStorage, Set<ConstraintViolation<T>> constraintViolations);
}
