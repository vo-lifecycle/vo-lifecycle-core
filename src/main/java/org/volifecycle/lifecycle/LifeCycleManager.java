package org.volifecycle.lifecycle;

import java.util.List;
import java.util.Map;

import org.volifecycle.event.EventManager;

/**
 * Manager interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            ValueObject type
 * @param <A>
 *            Adapter type
 */
public interface LifeCycleManager<T, A extends LifeCycleAdapter<T>> {
    /**
     * Run a transition on valueObject from a transition configuration id.
     * 
     * @param transitionId
     * @param valueObject
     * @return String "true" or "false"
     */
    String runTransition(String transitionId, T valueObject) throws IllegalStateException;

    /**
     * Run a transition on valueObject from a transition configuration id and
     * forced actions list.
     * 
     * @param idTransition
     * @param valueObject
     * @param forcedActions
     * @return String "false" if failure or state id if success
     */
    String runTransition(String idTransition, T valueObject, List<String> forcedActions) throws IllegalStateException;

    /**
     * Run a transition on valueObject from a transition configuration id and
     * forced actions list and pre-storage.
     * 
     * @param idTransition
     * @param valueObject
     * @param forcedActions
     * @param storage
     * @return String "false" if failure or state id if success
     */
    String runTransition(String idTransition, T valueObject, List<String> forcedActions, Map<String, Object> storage) throws IllegalStateException;

    /**
     * @return the description
     */
    String getDescription();

    /**
     * @return the id
     */
    String getId();

    /**
     * @return the saver
     */
    LifeCycleChangeSaver getSaver();

    /**
     * @return the evtManager
     */
    EventManager getEvtManager();

    /**
     * @return the adapter
     */
    A getAdapter();

    /**
     * @return the statesById
     */
    Map<String, LifeCycleState<T>> getStatesById();

    /**
     * Get all transitions from type (auto or manual).
     * 
     * @param type
     * @return Map<String, LifeCycleTransition<T>> with key that correspond to
     *         the transition's id
     */
    Map<String, LifeCycleTransition<T>> getTransitionsFromType(String type);

    /**
     * Get all transitions from type (auto or manual).
     * 
     * @param type
     * @param stateId
     * @return Map<String, LifeCycleTransition<T>> with key that correspond to
     *         the transition's id
     */
    Map<String, LifeCycleTransition<T>> getTransitionsFromType(String type, String stateId);

    /**
     * Get all transition's ids from type (auto or manual).
     * 
     * @param type
     * @return List<String>
     */
    List<String> getIdsTransitionsFromType(String type);

    /**
     * Get all transition's ids from type (auto or manual).
     * 
     * @param type
     * @param stateId
     * @return List<String>
     */
    List<String> getIdsTransitionsFromType(String type, String stateId);

    /**
     * Getting the additionnal informations for lifecycle.
     * 
     * @return Map<String, String>
     */
    Map<String, String> getAdditionnalInformations();
}
