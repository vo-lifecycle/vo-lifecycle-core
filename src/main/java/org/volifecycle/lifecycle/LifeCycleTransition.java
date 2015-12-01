package org.volifecycle.lifecycle;

import java.util.List;
import java.util.Map;

import org.volifecycle.event.EventManager;

/**
 * Transition interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject type
 * 
 */
public interface LifeCycleTransition<T> {
    /**
     * Verify all actions.
     * 
     * @param valueObject
     * @param adapter
     * @param evtManager
     * 
     * @return "true" if success or "false"
     */
    String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager);

    /**
     * Verify all actions with a forced id list.
     * 
     * @param valueObject
     * @param adapter
     * @param evtManager
     * @param forcedActions
     * @return the state's id if success or "false"
     */
    String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedActions);

    /**
     * Verify all actions with a forced id list and pre-storage.
     * 
     * @param valueObject
     * @param adapter
     * @param evtManager
     * @param forcedActions
     * @param storage
     * @return the state's id if success or "false"
     */
    String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedActions, Map<String, Object> storage);

    /**
     * @return the id
     */
    String getId();

    /**
     * @return the type
     */
    String getType();

    /**
     * @return the description
     */
    String getDescription();

    /**
     * @return the targetStates
     */
    List<String> getTargetStates();

    /**
     * @param targetStates
     *            the targetStates to set
     */
    void setTargetStates(List<String> targetStates);

    /**
     * Getting the additionnal informations for transition.
     * 
     * @return Map<String, String>
     */
    Map<String, String> getAdditionnalInformations();
}
