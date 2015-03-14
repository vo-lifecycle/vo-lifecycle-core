package org.volifecycle.event.vo;

import java.util.Calendar;

/**
 * Event value object class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class Event {
	/**
	 * Type of event (issue, etc)
	 */
	private String typeEvent;
	
	/**
	 * when ?
	 */
	private Calendar date;
	
	/**
	 * what ?
	 */
	private String message;
	
	/**
	 * who ?
	 */
	private String actor;

	/**
	 * @return the typeEvent
	 */
	public String getTypeEvent() {
		return typeEvent;
	}

	/**
	 * @param typeEvent the typeEvent to set
	 */
	public void setTypeEvent(String typeEvent) {
		this.typeEvent = typeEvent;
	}

	/**
	 * @return the date
	 */
	public Calendar getDate() {
		return date;
	}

	/**
	 * @param date the date to set
	 */
	public void setDate(Calendar date) {
		this.date = date;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @param message the message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * @return the actor
	 */
	public String getActor() {
		return actor;
	}

	/**
	 * @param actor the actor to set
	 */
	public void setActor(String actor) {
		this.actor = actor;
	}
}
