package org.volifecycle.lifecycle.impl;

import java.util.List;
import java.util.Map;

import org.volifecycle.constants.Constants;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.Log4jEventManagerImpl;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleManager;
import org.volifecycle.lifecycle.LifeCycleState;
import org.volifecycle.lifecycle.LifeCycleTransition;

/**
 * Implementation of manager
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T> valueObject type
 */
public class LifeCycleManagerImpl<T, A extends LifeCycleAdapter<T>> implements LifeCycleManager<T, LifeCycleAdapter<T>> {
    /**
     * Etats par ids
     */
    protected Map<String, LifeCycleState<T>> statesById;
    protected A adapter;
    protected EventManager evtManager;
    
    /**
     * Description
     */
    protected String description;

    /**
     * {@inheritDoc}
     */
    @Override
    public String runTransition(String idTransition, T valueObject, List<String> forcedCheckers) {
    	EventManager eManager = getEvtManager();
    	
    	// Default EventManager
    	if(null == eManager){
    		eManager = new Log4jEventManagerImpl();
    	}
    	
    	if (null == valueObject) {
            throw new IllegalStateException("Objet inexistant : " + valueObject);
        }

        String keyState = adapter.getState(valueObject);

        // Recherche de l'état courrant de l'objet dans la table de transco
        if (null == keyState || !statesById.containsKey(keyState)) {
            throw new IllegalStateException("Etat inexistant : " + keyState);
        }
        LifeCycleState<T> currentState = statesById.get(keyState);

        // Récupération de toutes les transitions associées à l'état courrant
        Map<String, LifeCycleTransition<T>> transitionsById = currentState.getTransitionsById();

        // Recherche de la transition demandée dans les transitions autorisées
        // pour l'object courrant
        if (null == transitionsById || !transitionsById.containsKey(idTransition)) {
            throw new IllegalStateException("Transition inexistante (" + idTransition + ") pour l'état " + currentState);
        }

        LifeCycleTransition<T> transition = transitionsById.get(idTransition);
        String rtn = transition.changeState(valueObject, eManager, forcedCheckers);
        String targetState = transition.getTarget();

        // Mise à jour en bdd
        if (null != targetState && !Constants.FALSE.equalsIgnoreCase(rtn)) {
            adapter.setState(valueObject, targetState);
        }

        return rtn;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String runTransition(String idTransition, T valueObject) {
        return runTransition(idTransition, valueObject, null);
    }

    /**
     * @return the statesById
     */
    public Map<String, LifeCycleState<T>> getStatesById() {
        return statesById;
    }

    /**
     * @param statesById the statesById to set
     */
    public void setStatesById(Map<String, LifeCycleState<T>> statesById) {
        this.statesById = statesById;
    }

    /**
     * @return the adapter
     */
    public A getAdapter() {
        return adapter;
    }

    /**
     * @param adapter the adapter to set
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
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

	/**
	 * @return the evtManager
	 */
	public EventManager getEvtManager() {
		return evtManager;
	}

	/**
	 * @param evtManager the evtManager to set
	 */
	public void setEvtManager(EventManager evtManager) {
		this.evtManager = evtManager;
	}
}
