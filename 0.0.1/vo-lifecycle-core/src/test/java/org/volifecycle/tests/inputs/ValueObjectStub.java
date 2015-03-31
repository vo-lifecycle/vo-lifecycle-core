package org.volifecycle.tests.inputs;

import java.io.Serializable;
import java.util.List;

/**
 * Value object class for tests
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class ValueObjectStub implements Serializable {
	private static final long serialVersionUID = 1L;

	private String state = "state";
	private String id = "id";
	private String type = "type";

	private Float nb;

	private SubValueObject subValueObject;
	private List<SubValueObject> lstChilds;

	/**
	 * @return the state
	 */
	public String getState() {
		return state;
	}

	/**
	 * @param state
	 *            the state to set
	 */
	public void setState(String state) {
		this.state = state;
	}

	/**
	 * @return the id
	 */
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
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * @return the nb
	 */
	public Float getNb() {
		return nb;
	}

	/**
	 * @param nb
	 *            the nb to set
	 */
	public void setNb(Float nb) {
		this.nb = nb;
	}

	public SubValueObject getSubValueObject() {
		return subValueObject;
	}

	public void setSubValueObject(SubValueObject subValueObject) {
		this.subValueObject = subValueObject;
	}

	public List<SubValueObject> getLstChilds() {
		return lstChilds;
	}

	public void setLstChilds(List<SubValueObject> lstChilds) {
		this.lstChilds = lstChilds;
	}
}
