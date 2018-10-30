import React from 'react';
import Select from 'react-select';

const Navbar = (props) => (
    <nav className="navbar navbar-expand-lg justify-content-between">
      <div className="navbrand">
        <span className="navbar-brand mb-0 h1" style={{ fontSize: '1.5em' }}>
            Type Zoo<sup>beta</sup>
        </span>
      </div>
      <div className="navactions">
        <ul className="navbar-nav">
          <li className="nav-item" style={{ width: '30em' }}>
            <Select
                isMulti
                placeholder="Pick some type features"
                closeMenuOnSelect={false}
                escapeClearsValue={true}
                hideSelectedOptions={false}
                onChange={(enabled) => { props.updateOptions(enabled); }}
                // TODO: calling jqconsole.Reset() onChange causes the
                // closeMenuOnSelect behaviour to not work, but this doesn't
                // handle all possible updates either (e.g. delete a label)
                onMenuClose={() => { props.resetConsole(); }}
                name="features"
                options={props.features}
                className="basic-multi-select"
                classNamePrefix="select"
            />
          </li>
        </ul>
      </div>
    </nav>
)

export default Navbar
