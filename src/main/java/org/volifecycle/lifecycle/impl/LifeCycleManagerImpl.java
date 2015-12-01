package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.MapUtils.isEmpty;
import static org.apache.commons.collections.MapUtils.isNotEmpty;
import static org.volifecycle.utils.DateUtils.getCurrentTime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.LogEventManagerImpl;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleChangeSaver;
import org.volifecycle.lifecycle.LifeCycleManager;
import org.volifecycle.lifecycle.LifeCycleState;
import org.volifecycle.lifecycle.LifeCycleTransition;
import org.volifecycle.lifecycle.vo.LifeCycleChange;

/**
 * Implementation of manager
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject type
 */
public class LifeCycleManagerImpl<T, A extends LifeCycleAdapter<T>> implements LifeCycleManager<T, LifeCycleAdapter<T>> {
    /**
     * States by id
     */
    protected Map<String, LifeCycleState<T>> statesById;

    /**
     * The adapter
     */
    protected A adapter;

    /**
     * The event manager
     */
    protected EventManager evtManager;

    /**
     * The life cycle change saver
     */
    protected LifeCycleChangeSaver saver;

    /**
     * Id
     */
    protected String id;

    /**
     * Description
     */
    protected String description;

    /**
     * Additionnal informations
     */
    protected Map<String, String> additionnalInformations;

    /**
     * {@inheritDoc}
     */
    @Override
    public String runTransition(String idTransition, T valueObject) throws IllegalStateException {
        return runTransition(idTransition, valueObject, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String runTransition(String idTransition, T valueObject, List<String> forcedActions) throws IllegalStateException {
        return runTransition(idTransition, valueObject, forcedActions, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String runTransition(String idTransition, T valueObject, List<String> forcedActions, Map<String, Object> storage) throws IllegalStateException {
        EventManager eManager = getEvtManager();

        // Default EventManager
        if (null == eManager) {
            eManager = new LogEventManagerImpl();
        }

        if (null == valueObject) {
            throw new IllegalStateException("Value object must not be null");
        }

        String keyState = adapter.getState(valueObject);

        // Searching current state in transco
        if (null == keyState || !statesById.containsKey(keyState)) {
            throw new IllegalStateException("The transition " + idTransition + " is not applicable for the state " + ((null == keyState) ? "<null>" : keyState));
        }

        LifeCycleState<T> currentState = statesById.get(keyState);

        // Searching the current state's transitions
        Map<String, LifeCycleTransition<T>> transitionsById = currentState.getTransitionsById();

        // Searching the requested id of transition in the current state's
        // allowed transitions
        if (null == transitionsById || !transitionsById.containsKey(idTransition)) {
            throw new IllegalStateException("Unknown transition (" + idTransition + ") for state " + keyState);
        }

        LifeCycleTransition<T> transition = transitionsById.get(idTransition);
        String targetState = transition.changeState(valueObject, adapter, eManager, forcedActions, storage);

        // Change state (in database or other persistence support)
        if (null != targetState && !Boolean.FALSE.toString().equalsIgnoreCase(targetState)) {
            adapter.setState(valueObject, targetState);
            logChangeCustom(valueObject, idTransition, keyState, adapter, targetState);
        }

        return targetState;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, LifeCycleTransition<T>> getTransitionsFromType(String type, String stateId) {
        Map<String, LifeCycleTransition<T>> rtn = new HashMap<String, LifeCycleTransition<T>>();
        if (isNotEmpty(statesById)) {
            for (Entry<String, LifeCycleState<T>> entryState : statesById.entrySet()) {
                LifeCycleState<T> state = entryState.getValue();
                if (null != stateId && !stateId.equalsIgnoreCase(entryState.getKey())) {
                    continue;
                }

                if (isNotEmpty(state.getTransitionsById())) {
                    for (Entry<String, LifeCycleTransition<T>> entryTransition : state.getTransitionsById().entrySet()) {
                        String idTransition = entryTransition.getKey();
                        LifeCycleTransition<T> transition = entryTransition.getValue();
                        if (!rtn.containsKey(idTransition) && type.equalsIgnoreCase(transition.getType())) {
                            rtn.put(idTransition, transition);
                        }
                    }
                }
            }
        }

        return rtn;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, LifeCycleTransition<T>> getTransitionsFromType(String type) {
        return getTransitionsFromType(type, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getIdsTransitionsFromType(String type, String stateId) {
        Map<String, LifeCycleTransition<T>> transitionsById = getTransitionsFromType(type, stateId);
        List<String> rtn = new ArrayList<String>();

        if (isNotEmpty(transitionsById)) {
            for (Entry<String, LifeCycleTransition<T>> entryTransition : transitionsById.entrySet()) {
                if (!rtn.contains(entryTransition.getKey())) {
                    rtn.add(entryTransition.getKey());
                }
            }
        }

        return rtn;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getIdsTransitionsFromType(String type) {
        return getIdsTransitionsFromType(type, null);
    }

    /**
     * Get states ids from transition type (manual or auto).
     * 
     * @param type
     * @return List<String>
     */
    public List<String> getStatesIdsFromTransitionType(String type) {
        List<String> idsTransition = getIdsTransitionsFromType(type);
        List<String> idsStates = new ArrayList<String>();

        if (isEmpty(idsTransition) || isEmpty(statesById)) {
            return idsStates;
        }

        for (Entry<String, LifeCycleState<T>> entry : statesById.entrySet()) {
            LifeCycleState<T> state = entry.getValue();
            String idState = entry.getKey();

            Map<String, LifeCycleTransition<T>> transitionsById = (null == state) ? null : state.getTransitionsById();
            if (isEmpty(transitionsById)) {
                continue;
            }

            for (Entry<String, LifeCycleTransition<T>> entry2 : transitionsById.entrySet()) {
                String idTransition = entry2.getKey();

                if (idsTransition.contains(idTransition) && !idsStates.contains(idState)) {
                    idsStates.add(idState);
                }
            }
        }

        return idsStates;
    }

    /**
     * Log change.
     * 
     * @param valueObject
     * @param transitionId
     * @param stateIn
     * @param adapter
     * @param stateOut
     */
    public void logChangeCustom(T valueObject, String transitionId, String stateIn, LifeCycleAdapter<T> adapter, String stateOut) {
        LifeCycleChangeSaver s = getSaver();
        if (null == s) {
            s = new LogLifeCycleChangeSaverImpl();
        }

        LifeCycleChange c = new LifeCycleChange();
        c.setDate(getCurrentTime());
        c.setStateIn(stateIn);
        c.setStateOut(stateOut);
        c.setValueObjectId(adapter.getId(valueObject));
        c.setValueObjectType(adapter.getType(valueObject));
        c.setTransitionId(transitionId);
        s.logChange(c);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, LifeCycleState<T>> getStatesById() {
        return statesById;
    }

    /**
     * @param statesById
     *            the statesById to set
     */
    public void setStatesById(Map<String, LifeCycleState<T>> statesById) {
        this.statesById = statesById;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public A getAdapter() {
        return adapter;
    }

    /**
     * @param adapter
     *            the adapter to set
     */
    public void setAdapter(A adapter) {
        this.adapter = adapter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventManager getEvtManager() {
        return evtManager;
    }

    /**
     * @param evtManager
     *            the evtManager to set
     */
    public void setEvtManager(EventManager evtManager) {
        this.evtManager = evtManager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public LifeCycleChangeSaver getSaver() {
        return saver;
    }

    /**
     * @param saver
     *            the saver to set
     */
    public void setSaver(LifeCycleChangeSaver saver) {
        this.saver = saver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> getAdditionnalInformations() {
        return additionnalInformations;
    }

    /**
     * @param additionnalInformations
     *            the additionnalInformations to set
     */
    public void setAdditionnalInformations(Map<String, String> additionnalInformations) {
        this.additionnalInformations = additionnalInformations;
    }
}
